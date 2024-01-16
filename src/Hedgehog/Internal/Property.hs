{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Property
  ( -- * Property
    Property (..),
    PropertyConfig (..),
    TestCount (..),
    DiscardCount (..),
    ShrinkCount (..),
    Skip (..),
    ShrinkPath (..),
    withTests,
    withDiscards,
    withShrinks,
    withSkip,
    property,
    forAll,
    forAllWith,
    defaultMinTests,
    discard,
    skipCompress,
    skipDecompress,

    -- * Group
    Group (..),
    PropertyCount (..),

    -- * TestT
    Test (..),
    Log (..),
    Journal (..),
    Failure (..),
    Diff (..),
    annotate,
    annotateShow,
    footnote,
    footnoteShow,
    diff,
    (===),
    (/==),
    eval,
    evalNF,
    evalEither,
    evalExceptT,
    evalMaybe,

    -- * Coverage
    Coverage (..),
    Label (..),
    LabelName (..),
    cover,
    classify,
    label,
    collect,
    coverPercentage,
    labelCovered,
    coverageSuccess,
    coverageFailures,
    journalCoverage,
    CoverCount (..),
    CoverPercentage (..),

    -- * Confidence
    Confidence (..),
    TerminationCriteria (..),
    confidenceSuccess,
    confidenceFailure,
    withConfidence,
    verifiedTermination,

    -- * Internal
    failWith,
    runTest,
  )
where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (SomeException (..), displayException)
import Control.Monad.Trans.Class (MonadTrans (..))
import Control.Monad.Trans.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans.Writer.Lazy qualified as Lazy
import Data.Char qualified as Char
import Data.Functor (($>))
import Data.Int (Int64)
import Data.List qualified as List
import Data.Map (Map)
import Data.Map.Strict qualified as Map
import Data.Number.Erf (invnormcdf)
import Data.Ratio ((%))
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Typeable (typeOf)
import GHC.Stack (HasCallStack, callStack, withFrozenCallStack)
import Hedgehog.Internal.Exception (tryEvaluate)
import Hedgehog.Internal.Gen (Gen)
import Hedgehog.Internal.Gen qualified as Gen
import Hedgehog.Internal.Show
import Hedgehog.Internal.Source (Span, getCaller)
import Numeric qualified
import Text.Read (readMaybe)
import Prelude

------------------------------------------------------------------------

-- | A property test, along with some configurable limits like how many times
--   to run the test.
data Property = Property
  { propertyConfig :: !PropertyConfig,
    propertyTest :: Test ()
  }

-- | A test monad allows the assertion of expectations.
newtype Test a = TestT
  { unTest :: ExceptT Failure (Lazy.WriterT Journal Gen) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad
    )

-- | The acceptable occurrence of false positives
--
--   Example, @Confidence 10^9@ would mean that you'd accept a false positive
--   for 1 in 10^9 tests.
newtype Confidence = Confidence
  { unConfidence :: Int64
  }
  deriving (Eq, Ord, Show, Num)

-- | Configuration for a property test.
data PropertyConfig = PropertyConfig
  { propertyDiscardLimit :: !Int,
    propertyShrinkLimit :: !Int,
    propertyTerminationCriteria :: !TerminationCriteria,
    -- | If this is 'Nothing', we take the Skip from the environment variable
    --   @HEDGEHOG_SKIP@.
    propertySkip :: Maybe Skip
  }
  deriving (Eq, Ord, Show)

-- | The number of tests a property ran successfully.
newtype TestCount
  = TestCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The number of tests a property had to discard.
newtype DiscardCount
  = DiscardCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | The numbers of times a property was able to shrink after a failing test.
newtype ShrinkCount
  = ShrinkCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

-- | Where to start running a property's tests.
data Skip
  = -- | Don't skip anything.
    SkipNothing
  | -- | Skip to a specific test number. If it fails, shrink as normal. If it
    --   passes, move on to the next test. Coverage checks are disabled.
    --
    --   We also need to count discards, since failing "after 7 tests" points at a
    --   different generated value than failing "after 7 tests and 5 discards".
    SkipToTest TestCount DiscardCount
  | -- | Skip to a specific test number and shrink state. If it fails, stop
    --   without shrinking further. If it passes, the property will pass without
    --   running any more tests.
    --
    --   Due to implementation details, all intermediate shrink states - those on
    --   the direct path from the original test input to the target state - will
    --   be tested too, and their results discarded.
    SkipToShrink TestCount DiscardCount ShrinkPath
  deriving (Eq, Ord, Show)

-- | We use this instance to support usage like
--
-- @
--   withSkip "3:aB"
-- @
--
--   It throws an error if the input is not a valid compressed 'Skip'.
instance IsString Skip where
  fromString s =
    case skipDecompress s of
      Nothing -> error $ "fromString: Not a valid Skip: " ++ s
      Just skip -> skip

-- | The path taken to reach a shrink state.
newtype ShrinkPath
  = ShrinkPath [Int]
  deriving (Eq, Ord, Show)

-- | Compress a Skip into a hopefully-short alphanumeric string.
--
--   The bit that might be long is the 'ShrinkPath' in 'SkipToShrink'. For that,
--   we encode the path components in base 26, alternating between uppercase and
--   lowercase alphabets to distinguish list elements. Additionally when we have
--   runs of equal components, we use the normal base 10 encoding to indicate
--   the length.
--
--   This gives something which is hopefully quite short, but a human can
--   roughly interpret it by eyeball.
skipCompress :: Skip -> String
skipCompress = \case
  SkipNothing -> ""
  SkipToTest t d -> showTD t d
  SkipToShrink t d sp -> showTD t d ++ ":" ++ shrinkPathCompress sp
  where
    showTD (TestCount t) (DiscardCount d) =
      show t ++ (if d == 0 then "" else "/" ++ show d)

-- | Compress a 'ShrinkPath' into a hopefully-short alphanumeric string.
--
--   We encode the path components in base 26, alternating between uppercase and
--   lowercase alphabets to distinguish list elements. Additionally when we have
--   runs of equal components, we use the normal base 10 encoding to indicate
--   the length.
shrinkPathCompress :: ShrinkPath -> String
shrinkPathCompress (ShrinkPath sp) =
  let groups = List.map (\l -> (head l, length l)) $ List.group sp
   in ( mconcat $
          zipWith
            ( \alphabet (loc, count) ->
                Numeric.showIntAtBase 26 (alphabet !!) loc
                  <> if count == 1 then mempty else shows count
            )
            (cycle [['a' .. 'z'], ['A' .. 'Z']])
            groups
      )
        ""

-- | Decompress a 'Skip'.
--
--   This satisfies
--
-- @
--   skipDecompress (skipCompress a) == Just a
-- @
skipDecompress :: String -> Maybe Skip
skipDecompress str =
  if null str
    then Just SkipNothing
    else do
      let (tcDcStr, spStr) =
            span (/= ':') str

          (tcStr, dcStr) =
            span (/= '/') tcDcStr

      tc <- TestCount <$> readMaybe tcStr
      dc <-
        DiscardCount
          <$> if null dcStr
            then Just 0
            else readMaybe (drop 1 dcStr)

      if null spStr
        then Just $ SkipToTest tc dc
        else do
          sp <- shrinkPathDecompress $ drop 1 spStr
          Just $ SkipToShrink tc dc sp

-- | Decompress a 'ShrinkPath'.
--
--   This satisfies
--
-- @
--   shrinkPathDecompress (shrinkPathCompress a) == Just a
-- @
shrinkPathDecompress :: String -> Maybe ShrinkPath
shrinkPathDecompress str =
  let isDigit c = '0' <= c && c <= '9'
      isLower c = 'a' <= c && c <= 'z'
      isUpper c = 'A' <= c && c <= 'Z'
      classifyChar c = (isDigit c, isLower c, isUpper c)

      readSNum "" = []
      readSNum s@(c1 : _) =
        if isDigit c1
          then Numeric.readInt 10 isDigit (\c -> fromEnum c - fromEnum '0') s
          else
            if isLower c1
              then Numeric.readInt 26 isLower (\c -> fromEnum c - fromEnum 'a') s
              else
                if isUpper c1
                  then Numeric.readInt 26 isUpper (\c -> fromEnum c - fromEnum 'A') s
                  else []

      readNumMaybe s =
        case readSNum s of
          [(num, "")] -> Just num
          _ -> Nothing

      spGroups :: [(Maybe Int, Maybe Int)] =
        let go [] = []
            go (c1 : cs) =
              let (hd, tl1) =
                    span (\c -> classifyChar c == classifyChar c1) cs
                  (digs, tl2) =
                    span isDigit tl1
               in ( readNumMaybe (c1 : hd),
                    readNumMaybe $ if null digs then "1" else digs
                  )
                    : go tl2
         in go str
   in do
        sp <-
          concat
            <$> traverse (\(mNum, mCount) -> replicate <$> mCount <*> mNum) spGroups
        Just $ ShrinkPath sp

-- | A named collection of property tests.
data Group = Group
  { groupName :: !Text,
    groupProperties :: ![(Text, Property)]
  }

-- | The number of properties in a group.
newtype PropertyCount
  = PropertyCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

data TerminationCriteria
  = EarlyTermination Confidence Int
  | NoEarlyTermination Confidence Int
  | NoConfidenceTermination Int
  deriving (Eq, Ord, Show)

--
-- FIXME This whole Log/Failure thing could be a lot more structured to allow
-- FIXME for richer user controlled error messages, think Doc. Ideally we'd
-- FIXME allow user's to crete their own diffs anywhere.
--

-- | Log messages which are recorded during a test run.
data Log
  = Annotation (Maybe Span) String
  | Footnote String
  | Label (Label Cover)
  deriving (Eq, Show)

-- | A record containing the details of a test run.
newtype Journal = Journal
  { journalLogs :: [Log]
  }
  deriving (Eq, Show, Semigroup, Monoid)

-- | Details on where and why a test failed.
data Failure
  = Failure (Maybe Span) String (Maybe Diff)
  deriving (Eq, Show)

-- | The difference between some expected and actual value.
data Diff = Diff
  { diffPrefix :: String,
    diffRemoved :: String,
    diffInfix :: String,
    diffAdded :: String,
    diffSuffix :: String,
    diffValue :: ValueDiff
  }
  deriving (Eq, Show)

-- | Whether a test is covered by a classifier, and therefore belongs to a
--   'Class'.
data Cover
  = NoCover
  | Cover
  deriving (Eq, Ord, Show)

-- | The total number of tests which are covered by a classifier.
--
--   Can be constructed using numeric literals:
--
-- @
--   30 :: CoverCount
-- @
newtype CoverCount = CoverCount
  { unCoverCount :: Int
  }
  deriving (Eq, Ord, Show, Num)

-- | The relative number of tests which are covered by a classifier.
--
--   Can be constructed using numeric literals:
--
-- @
--   30 :: CoverPercentage
-- @
newtype CoverPercentage = CoverPercentage
  { unCoverPercentage :: Double
  }
  deriving (Eq, Ord, Show, Num, Fractional)

-- | The name of a classifier.
--
--   Should be constructed using `OverloadedStrings`:
--
-- @
--   "apples" :: LabelName
-- @
newtype LabelName = LabelName
  { unLabelName :: String
  }
  deriving (Eq, Monoid, Ord, Semigroup, Show, IsString)

-- | The extent to which a test is covered by a classifier.
--
--   /When a classifier's coverage does not exceed the required minimum, the/
--   /test will be failed./
data Label a = MkLabel
  { labelName :: !LabelName,
    labelLocation :: !(Maybe Span),
    labelMinimum :: !CoverPercentage,
    labelAnnotation :: !a
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

-- | The extent to which all classifiers cover a test.
--
--   /When a given classification's coverage does not exceed the required/
--   /minimum, the test will be failed./
newtype Coverage a = Coverage
  { coverageLabels :: Map LabelName (Label a)
  }
  deriving (Eq, Show, Functor, Foldable, Traversable)

------------------------------------------------------------------------
-- TestT

instance MonadFail Test where
  fail err =
    TestT . ExceptT . pure . Left $ Failure Nothing err Nothing

mkTest :: Gen (Either Failure a, Journal) -> Test a
mkTest =
  TestT . ExceptT . Lazy.WriterT

runTest :: Test a -> Gen (Either Failure a, Journal)
runTest =
  Lazy.runWriterT . runExceptT . unTest

-- | Log some information which might be relevant to a potential test failure.
writeLog :: Log -> Test ()
writeLog x =
  mkTest (pure (pure (), (Journal [x])))

-- | Fail the test with an error message, useful for building other failure
--   combinators.
failWith :: (HasCallStack) => Maybe Diff -> String -> Test a
failWith mdiff msg =
  mkTest (pure (Left $ Failure (getCaller callStack) msg mdiff, mempty))

-- | Annotates the source code with a message that might be useful for
--   debugging a test failure.
annotate :: (HasCallStack) => String -> Test ()
annotate x = do
  writeLog $ Annotation (getCaller callStack) x

-- | Annotates the source code with a value that might be useful for
--   debugging a test failure.
annotateShow :: (Show a, HasCallStack) => a -> Test ()
annotateShow x = do
  withFrozenCallStack $ annotate (showPretty x)

-- | Logs a message to be displayed as additional information in the footer of
--   the failure report.
footnote :: String -> Test ()
footnote =
  writeLog . Footnote

-- | Logs a value to be displayed as additional information in the footer of
--   the failure report.
footnoteShow :: (Show a) => a -> Test ()
footnoteShow =
  writeLog . Footnote . showPretty

-- | Fails with an error that shows the difference between two values.
failDiff :: (Show a, Show b, HasCallStack) => a -> b -> Test ()
failDiff x y =
  case valueDiff <$> mkValue x <*> mkValue y of
    Nothing ->
      withFrozenCallStack $
        failWith Nothing $
          unlines $
            [ "Failed",
              "━━ lhs ━━",
              showPretty x,
              "━━ rhs ━━",
              showPretty y
            ]
    Just vdiff@(ValueSame _) ->
      withFrozenCallStack $
        failWith
          ( Just $
              Diff "━━━ Failed (" "" "no differences" "" ") ━━━" vdiff
          )
          ""
    Just vdiff ->
      withFrozenCallStack $
        failWith
          ( Just $
              Diff "━━━ Failed (" "- lhs" ") (" "+ rhs" ") ━━━" vdiff
          )
          ""

-- | Fails with an error which renders the type of an exception and its error
--   message.
failException :: (HasCallStack) => SomeException -> Test a
failException x =
  withFrozenCallStack $
    failExceptionWith [] x

-- | Fails with an error which renders the given messages, the type of an exception,
--   and its error message.
failExceptionWith :: (HasCallStack) => [String] -> SomeException -> Test a
failExceptionWith messages (SomeException x) =
  withFrozenCallStack
    failWith
    Nothing
    $ unlines
    $ messages
      <> [ "━━━ Exception (" ++ show (typeOf x) ++ ") ━━━",
           List.dropWhileEnd Char.isSpace (displayException x)
         ]

-- | Fails the test and shows a git-like diff if the comparison operation
--   evaluates to 'False' when applied to its arguments.
--
--   The comparison function is the second argument, which may be
--   counter-intuitive to Haskell programmers. However, it allows operators to
--   be written infix for easy reading:
--
-- @
--   diff y (<) 87
--   diff x (<=) 'r'
-- @
--
--   This function behaves like the unix @diff@ tool, which gives a 0 exit
--   code if the compared files are identical, or a 1 exit code code
--   otherwise. Like unix @diff@, if the arguments fail the comparison, a
--   /diff is shown.
diff :: (Show a, Show b, HasCallStack) => a -> (a -> b -> Bool) -> b -> Test ()
diff x op y = do
  ok <- withFrozenCallStack $ eval (x `op` y)
  if ok
    then pure ()
    else withFrozenCallStack $ failDiff x y

infix 4 ===

-- | Fails the test if the two arguments provided are not equal.
(===) :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
(===) x y =
  withFrozenCallStack $
    diff x (==) y

infix 4 /==

-- | Fails the test if the two arguments provided are equal.
(/==) :: (Eq a, Show a, HasCallStack) => a -> a -> Test ()
(/==) x y =
  withFrozenCallStack $
    diff x (/=) y

-- | Fails the test if the value throws an exception when evaluated to weak
--   head normal form (WHNF).
eval :: (HasCallStack) => a -> Test a
eval x =
  either (withFrozenCallStack failException) pure (tryEvaluate x)

-- | Fails the test if the value throws an exception when evaluated to
--   normal form (NF).
evalNF :: (NFData a, HasCallStack) => a -> Test a
evalNF x =
  let messages =
        ["━━━ Value could not be evaluated to normal form ━━━"]
   in either (withFrozenCallStack (failExceptionWith messages)) pure (tryEvaluate (rnf x)) $> x

-- | Fails the test if the 'Either' is 'Left', otherwise returns the value in
--   the 'Right'.
evalEither :: (Show x, HasCallStack) => Either x a -> Test a
evalEither = \case
  Left x -> withFrozenCallStack $ failWith Nothing $ showPretty x
  Right x -> pure x

-- | Fails the test if the 'ExceptT' is 'Left', otherwise returns the value in
--   the 'Right'.
evalExceptT :: (Show x, HasCallStack) => ExceptT x Test a -> Test a
evalExceptT m =
  withFrozenCallStack evalEither =<< runExceptT m

-- | Fails the test if the 'Maybe' is 'Nothing', otherwise returns the value in
--   the 'Just'.
evalMaybe :: (Show a, HasCallStack) => Maybe a -> Test a
evalMaybe = \case
  Nothing -> withFrozenCallStack $ failWith Nothing "the value was Nothing"
  Just x -> pure x

------------------------------------------------------------------------
-- PropertyT

-- | Generates a random input for the test by running the provided generator.
--
--   /This is a the same as 'forAllT' but allows the user to provide a custom/
--   /rendering function. This is useful for values which don't have a/
--   /'Show' instance./
forAllWith :: (HasCallStack) => (a -> String) -> Gen a -> Test a
forAllWith render gen = do
  x <- TestT (lift (lift gen))
  withFrozenCallStack $ annotate (render x)
  pure x

-- | Generates a random input for the test by running the provided generator.
forAll :: (Show a, HasCallStack) => Gen a -> Test a
forAll gen =
  withFrozenCallStack $ forAllWith showPretty gen

-- | Discards the current test entirely.
discard :: Test a
discard =
  TestT (lift (lift Gen.discard))

------------------------------------------------------------------------
-- Property

-- | The default configuration for a property test.
defaultConfig :: PropertyConfig
defaultConfig =
  PropertyConfig
    { propertyDiscardLimit = 100,
      propertyShrinkLimit = 1000,
      propertyTerminationCriteria = NoConfidenceTermination defaultMinTests,
      propertySkip = Nothing
    }

-- | The minimum amount of tests to run for a 'Property'
defaultMinTests :: Int
defaultMinTests = 100

-- | The default confidence allows one false positive in 10^9 tests
defaultConfidence :: Confidence
defaultConfidence = 10 ^ (9 :: Int)

-- | Map a config modification function over a property.
mapConfig :: (PropertyConfig -> PropertyConfig) -> Property -> Property
mapConfig f (Property cfg t) =
  Property (f cfg) t

-- | Make sure that the result is statistically significant in accordance to
--   the passed 'Confidence'
withConfidence :: Confidence -> Property -> Property
withConfidence c =
  let setConfidence = \case
        NoEarlyTermination _ tests -> NoEarlyTermination c tests
        NoConfidenceTermination tests -> NoEarlyTermination c tests
        EarlyTermination _ tests -> EarlyTermination c tests
   in mapConfig $ \config@PropertyConfig {..} ->
        config
          { propertyTerminationCriteria =
              setConfidence propertyTerminationCriteria
          }

verifiedTermination :: Property -> Property
verifiedTermination =
  mapConfig $ \config@PropertyConfig {..} ->
    let newTerminationCriteria = case propertyTerminationCriteria of
          NoEarlyTermination c tests -> EarlyTermination c tests
          NoConfidenceTermination tests -> EarlyTermination defaultConfidence tests
          EarlyTermination c tests -> EarlyTermination c tests
     in config {propertyTerminationCriteria = newTerminationCriteria}

-- | Set the number of times a property should be executed before it is considered
--   successful.
--
--   If you have a test that does not involve any generators and thus does not
--   need to run repeatedly, you can use @withTests 1@ to define a property that
--   will only be checked once.
withTests :: Int -> Property -> Property
withTests n =
  let setTestLimit tests = \case
        NoEarlyTermination c _ -> NoEarlyTermination c tests
        NoConfidenceTermination _ -> NoConfidenceTermination tests
        EarlyTermination c _ -> EarlyTermination c tests
   in mapConfig $ \config@PropertyConfig {..} ->
        config {propertyTerminationCriteria = setTestLimit n propertyTerminationCriteria}

-- | Set the number of times a property is allowed to discard before the test
--   runner gives up.
withDiscards :: Int -> Property -> Property
withDiscards n =
  mapConfig $ \config -> config {propertyDiscardLimit = n}

-- | Set the number of times a property is allowed to shrink before the test
--   runner gives up and prints the counterexample.
withShrinks :: Int -> Property -> Property
withShrinks n =
  mapConfig $ \config -> config {propertyShrinkLimit = n}

-- | Set the target that a property will skip to before it starts to run.
withSkip :: Skip -> Property -> Property
withSkip s =
  mapConfig $ \config -> config {propertySkip = Just s}

-- | Creates a property with the default configuration.
property :: (HasCallStack) => Test () -> Property
property m =
  Property defaultConfig $
    withFrozenCallStack m

------------------------------------------------------------------------
-- Coverage

instance Semigroup Cover where
  NoCover <> NoCover = NoCover
  _ <> _ = Cover

instance Monoid Cover where
  mempty = NoCover

instance Semigroup CoverCount where
  CoverCount n0 <> CoverCount n1 =
    CoverCount (n0 + n1)

instance Monoid CoverCount where
  mempty = CoverCount 0

toCoverCount :: Cover -> CoverCount
toCoverCount = \case
  NoCover -> CoverCount 0
  Cover -> CoverCount 1

-- | This semigroup is right biased. The name, location and percentage from the
--   rightmost `Label` will be kept. This shouldn't be a problem since the
--   library doesn't allow setting multiple classes with the same 'ClassifierName'.
instance (Semigroup a) => Semigroup (Label a) where
  MkLabel _ _ _ m0 <> MkLabel name location percentage m1 =
    MkLabel name location percentage (m0 <> m1)

instance (Semigroup a) => Semigroup (Coverage a) where
  Coverage c0 <> Coverage c1 =
    Coverage (Map.foldrWithKey (Map.insertWith (<>)) c0 c1)

instance (Semigroup a, Monoid a) => Monoid (Coverage a) where
  mempty = Coverage mempty

coverPercentage :: TestCount -> CoverCount -> CoverPercentage
coverPercentage (TestCount tests) (CoverCount count) =
  let percentage :: Double
      percentage =
        fromIntegral count / fromIntegral tests * 100

      thousandths :: Int
      thousandths =
        round $ percentage * 10
   in CoverPercentage (fromIntegral thousandths / 10)

labelCovered :: TestCount -> Label CoverCount -> Bool
labelCovered tests (MkLabel _ _ minimum_ population) =
  coverPercentage tests population >= minimum_

-- | All labels are covered
coverageSuccess :: TestCount -> Coverage CoverCount -> Bool
coverageSuccess tests =
  null . coverageFailures tests

coverageFailures :: TestCount -> Coverage CoverCount -> [Label CoverCount]
coverageFailures tests (Coverage kvs) =
  List.filter (not . labelCovered tests) (Map.elems kvs)

-- | Is true when the test coverage satisfies the specified 'Confidence'
--   contstraint for all 'Coverage CoverCount's
confidenceSuccess :: TestCount -> Confidence -> Coverage CoverCount -> Bool
confidenceSuccess tests confidence =
  let assertLow :: Label CoverCount -> Bool
      assertLow coverCount@MkLabel {..} =
        fst (boundsForLabel tests confidence coverCount)
          >= unCoverPercentage labelMinimum / 100.0
   in and . fmap assertLow . Map.elems . coverageLabels

-- | Is true when there exists a label that is sure to have failed according to
--   the 'Confidence' constraint
confidenceFailure :: TestCount -> Confidence -> Coverage CoverCount -> Bool
confidenceFailure tests confidence =
  let assertHigh :: Label CoverCount -> Bool
      assertHigh coverCount@MkLabel {..} =
        snd (boundsForLabel tests confidence coverCount)
          < (unCoverPercentage labelMinimum / 100.0)
   in or . fmap assertHigh . Map.elems . coverageLabels

boundsForLabel :: TestCount -> Confidence -> Label CoverCount -> (Double, Double)
boundsForLabel tests confidence MkLabel {..} =
  wilsonBounds
    (fromIntegral $ unCoverCount labelAnnotation)
    (fromIntegral tests)
    (1 / fromIntegral (unConfidence confidence))

-- In order to get an accurate measurement with small sample sizes, we're
-- using the Wilson score interval
-- (<https://en.wikipedia.org/wiki/Binomial_proportion_confidence_interval#Wilson_score_interval
-- wikipedia>) instead of a normal approximation interval.
wilsonBounds :: Integer -> Integer -> Double -> (Double, Double)
wilsonBounds positives count acceptance =
  let p = fromRational $ positives % count
      n = fromIntegral count
      z = invnormcdf $ 1 - acceptance / 2
      midpoint = p + z * z / (2 * n)
      offset = z / (1 + z ** 2 / n) * sqrt (p * (1 - p) / n + z ** 2 / (4 * n ** 2))
      denominator = 1 + z * z / n
      low = (midpoint - offset) / denominator
      high = (midpoint + offset) / denominator
   in (low, high)

fromLabel :: Label a -> Coverage a
fromLabel x =
  Coverage (Map.singleton (labelName x) x)

unionsCoverage :: (Semigroup a) => [Coverage a] -> Coverage a
unionsCoverage =
  Coverage . Map.unionsWith (<>) . fmap coverageLabels

journalCoverage :: Journal -> Coverage CoverCount
journalCoverage (Journal logs) =
  (fmap toCoverCount . unionsCoverage) do
    Label x <- logs
    pure (fromLabel x)

-- | Require a certain percentage of the tests to be covered by the
--   classifier.
--
-- @
--    prop_with_coverage :: Property
--    prop_with_coverage =
--      property $ do
--        match <- forAll Gen.bool
--        cover 30 \"True\" $ match
--        cover 30 \"False\" $ not match
-- @
--
--   The example above requires a minimum of 30% coverage for both
--   classifiers. If these requirements are not met, it will fail the test.
cover :: (HasCallStack) => CoverPercentage -> LabelName -> Bool -> Test ()
cover minimum_ name covered =
  let cover_ =
        if covered
          then Cover
          else NoCover
   in writeLog . Label $
        MkLabel name (getCaller callStack) minimum_ cover_

-- | Records the proportion of tests which satisfy a given condition.
--
-- @
--    prop_with_classifier :: Property
--    prop_with_classifier =
--      property $ do
--        xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
--        for_ xs $ \\x -> do
--          classify "newborns" $ x == 0
--          classify "children" $ x > 0 && x < 13
--          classify "teens" $ x > 12 && x < 20
-- @
classify :: (HasCallStack) => LabelName -> Bool -> Test ()
classify name covered =
  withFrozenCallStack $
    cover 0 name covered

-- | Add a label for each test run. It produces a table showing the percentage
--   of test runs that produced each label.
label :: (HasCallStack) => LabelName -> Test ()
label name =
  withFrozenCallStack $
    cover 0 name True

-- | Like 'label', but uses 'Show' to render its argument for display.
collect :: (Show a, HasCallStack) => a -> Test ()
collect x =
  withFrozenCallStack $
    cover 0 (LabelName (show x)) True

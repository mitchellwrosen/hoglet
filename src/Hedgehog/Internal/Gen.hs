{-# OPTIONS_HADDOCK not-home #-}
{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DerivingVia #-}

module Hedgehog.Internal.Gen (
  -- * Transformer
    Gen
  , evalGen

  -- ** Shrinking
  , shrink
  , prune

  -- ** Size
  , small
  , scale
  , resize
  , sized

  -- ** Integral
  , integral
  , integral_

  , int
  , int8
  , int16
  , int32
  , int64

  , word
  , word8
  , word16
  , word32
  , word64

  -- ** Floating-point
  , realFloat
  , realFrac_
  , float
  , double

  -- ** Enumeration
  , enum
  , enumBounded
  , bool
  , bool_

  -- ** Characters
  , binit
  , octit
  , digit
  , hexit
  , lower
  , upper
  , alpha
  , alphaNum
  , ascii
  , latin1
  , unicode
  , unicodeAll

  -- ** Strings
  , string
  , text
  , utf8
  , bytes

  -- ** Choice
  , constant
  , element
  , element_
  , choice
  , frequency
  , recursive

  -- ** Conditional
  , discard
  , ensure
  , filter
  , mapMaybe
  , filterT
  , mapMaybeT
  , just
  , justT

  -- ** Collections
  , maybe
  , either
  , either_
  , list
  , seq
  , nonEmpty
  , set
  , map

  -- ** Subterms
  , freeze
  , subterm
  , subtermM
  , subterm2
  , subtermM2
  , subterm3
  , subtermM3

  -- ** Combinations & Permutations
  , subsequence
  , subset
  , shuffle

  -- * Sampling Generators
  , sample
  , print
  , printTree
  , printWith
  , printTreeWith
  ) where

#if !MIN_VERSION_base(4,18,0)
import           Control.Applicative (liftA2)
#endif
import           Control.Applicative (Alternative(..))
import           Control.Monad (filterM, guard, replicateM)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Morph (MFunctor(..))
import qualified Control.Monad.Morph as Morph

import           Data.Bifunctor (first)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import qualified Data.Char as Char
import           Data.Foldable (for_, toList)
import           Data.Functor.Identity (Identity(..))
import           Data.Int (Int8, Int16, Int32, Int64)
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Semigroup as Semigroup
import           Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import           Data.Word (Word8, Word16, Word32, Word64)

import           Hedgehog.Internal.Range (Size, Range)
import qualified Hedgehog.Internal.Range as Range
import           Hedgehog.Internal.Seed (Seed)
import qualified Hedgehog.Internal.Seed as Seed
import qualified Hedgehog.Internal.Shrink as Shrink
import           Hedgehog.Internal.Tree (Tree, TreeT(..), NodeT(..))
import qualified Hedgehog.Internal.Tree as Tree

import           Prelude hiding (either, filter, map, maybe, print, seq)


------------------------------------------------------------------------
-- Generator transformer

-- | Generator for random values of @a@.
--
newtype Gen a =
  Gen (Size -> Seed -> TreeT Maybe a)

-- | Runs a generator, producing its shrink tree.
--
runGen :: Size -> Seed -> Gen a -> TreeT Maybe a
runGen size seed (Gen m) =
  m size seed

-- | Run a generator, producing its shrink tree.
--
--   'Nothing' means discarded, 'Just' means we have a value.
--
evalGen :: Size -> Seed -> Gen a -> Maybe (Tree a)
evalGen size seed =
  Tree.runTreeMaybeT .
  runGen size seed

-- | Map over a generator's shrink tree.
--
mapGen :: (TreeT Maybe a -> TreeT Maybe b) -> Gen a -> Gen b
mapGen f gen =
  Gen $ \size seed ->
    f (runGen size seed gen)

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
fromTreeMaybeT :: TreeT Maybe a -> Gen a
fromTreeMaybeT x =
  Gen $ \_ _ ->
    x

-- | Lift a predefined shrink tree in to a generator, ignoring the seed and the
--   size.
--
toTreeMaybeT :: Gen a -> Gen (TreeT Maybe a)
toTreeMaybeT =
  mapGen pure

------------------------------------------------------------------------
-- Gen instances

instance (Semigroup a) => Semigroup (Gen a) where
  (<>) =
    liftA2 (Semigroup.<>)

instance (
  Monoid a
#if !MIN_VERSION_base(4,11,0)
  , Semigroup a
#endif
         ) => Monoid (Gen a) where
#if !MIN_VERSION_base(4,11,0)
  mappend = (Semigroup.<>)
#endif

  mempty =
    return mempty

instance Functor Gen where
  fmap f gen =
    Gen $ \seed size ->
      fmap f (runGen seed size gen)

--
-- implementation: parallel shrinking
--
instance Applicative Gen where
  pure =
    fromTreeMaybeT . pure

  (<*>) f m =
    Gen $ \size seed ->
      case Seed.split seed of
        (sf, sm) ->
          uncurry ($) <$>
            runGen size sf f `Tree.zipTreeT`
            runGen size sm m

--
-- implementation: satisfies law (ap = <*>)
--
--instance Monad m => Applicative (GenT m) where
--  pure =
--    fromTreeMaybeT . pure
--  (<*>) f m =
--    GenT $ \ size seed ->
--      case Seed.split seed of
--        (sf, sm) ->
--          runGenT size sf f <*>
--          runGenT size sm m

instance Monad Gen where
  return =
    pure

  (>>=) m k =
    Gen $ \size seed ->
      case Seed.split seed of
        (sk, sm) ->
          runGen size sk . k =<<
          runGen size sm m

instance MonadFail Gen where
  fail =
    error

instance Alternative Gen where
  empty =
    fromTreeMaybeT empty

  x <|> y =
    Gen $ \size seed ->
      case Seed.split seed of
        (sx, sy) ->
          runGen size sx x <|>
          runGen size sy y

------------------------------------------------------------------------
-- Combinators

-- | Generate a value with no shrinks from a 'Size' and a 'Seed'.
--
generate :: (Size -> Seed -> a) -> Gen a
generate f =
  Gen $ \size seed ->
    pure (f size seed)

------------------------------------------------------------------------
-- Combinators - Shrinking

-- | Apply a shrinking function to a generator.
--
--   This will give the generator additional shrinking options, while keeping
--   the existing shrinks intact.
--
shrink :: (a -> [a]) -> Gen a -> Gen a
shrink f =
  mapGen (Tree.expand f)

-- | Throw away a generator's shrink tree.
--
prune :: Gen a -> Gen a
prune =
  mapGen (Tree.prune 0)

------------------------------------------------------------------------
-- Combinators - Size

-- | Construct a generator that depends on the size parameter.
--
sized :: (Size -> Gen a) -> Gen a
sized f = do
  f =<< generate (\size _ -> size)

-- | Override the size parameter. Returns a generator which uses the given size
--   instead of the runtime-size parameter.
--
resize :: Size -> Gen a -> Gen a
resize size gen =
  scale (const size) gen

-- | Adjust the size parameter by transforming it with the given function.
--
scale :: (Size -> Size) -> Gen a -> Gen a
scale f =
  \gen ->
    Gen $ \size0 seed ->
      let
        size =
          f size0
      in
        if size < 0 then
          error "Hedgehog.Gen.scale: negative size"
        else
          runGen size seed gen

-- | Make a generator smaller by scaling its size parameter.
--
small :: Gen a -> Gen a
small =
  scale golden

-- | Scale a size using the golden ratio.
--
--   > golden x = x / φ
--   > golden x = x / 1.61803..
--
golden :: Size -> Size
golden x =
  round (fromIntegral x * 0.61803398875 :: Double)

------------------------------------------------------------------------
-- Combinators - Integral

-- | Generates a random integral number in the given @[inclusive,inclusive]@ range.
--
--   When the generator tries to shrink, it will shrink towards the
--   'Range.origin' of the specified 'Range'.
--
--   For example, the following generator will produce a number between @1970@
--   and @2100@, but will shrink towards @2000@:
--
-- @
-- integral (Range.'Range.constantFrom' 2000 1970 2100) :: 'Gen' 'Int'
-- @
--
--   Some sample outputs from this generator might look like:
--
--   > === Outcome ===
--   > 1973
--   > === Shrinks ===
--   > 2000
--   > 1987
--   > 1980
--   > 1976
--   > 1974
--
--   > === Outcome ===
--   > 2061
--   > === Shrinks ===
--   > 2000
--   > 2031
--   > 2046
--   > 2054
--   > 2058
--   > 2060
--
integral :: forall a. (Integral a) => Range a -> Gen a
integral range =
  -- https://github.com/hedgehogqa/haskell-hedgehog/pull/413/files
  let
    origin_ =
      Range.origin range

    binarySearchTree bottom top =
      Tree.Tree $
        let
          shrinks =
            Shrink.towards bottom top
          children =
            zipWith binarySearchTree shrinks (drop 1 shrinks)
        in
          Tree.NodeT top children

    createTree root =
      if root == origin_ then
        pure root
      else
        hoist Morph.generalize $
          Tree.consChild origin_ $
            binarySearchTree origin_ root

  in
    Gen $ \size seed ->
      createTree $ integralHelper range size seed

-- | Generates a random integral number in the [inclusive,inclusive] range.
--
--   /This generator does not shrink./
--
integral_ :: (Integral a) => Range a -> Gen a
integral_ =
  generate . integralHelper


-- | Generates a random integral value from a range.
integralHelper :: (Integral a, Num c) => Range a -> Size -> Seed -> c
integralHelper range size seed =
  let
    (x, y) =
      Range.bounds size range
  in
    fromInteger . fst $
      Seed.nextInteger (toInteger x) (toInteger y) seed


-- | Generates a random machine integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int :: Range Int -> Gen Int
int =
  integral

-- | Generates a random 8-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int8 :: Range Int8 -> Gen Int8
int8 =
  integral

-- | Generates a random 16-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int16 :: Range Int16 -> Gen Int16
int16 =
  integral

-- | Generates a random 32-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int32 :: Range Int32 -> Gen Int32
int32 =
  integral

-- | Generates a random 64-bit integer in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
int64 :: Range Int64 -> Gen Int64
int64 =
  integral

-- | Generates a random machine word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word :: Range Word -> Gen Word
word =
  integral

-- | Generates a random byte in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word8 :: Range Word8 -> Gen Word8
word8 =
  integral

-- | Generates a random 16-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word16 :: Range Word16 -> Gen Word16
word16 =
  integral

-- | Generates a random 32-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word32 :: Range Word32 -> Gen Word32
word32 =
  integral

-- | Generates a random 64-bit word in the given @[inclusive,inclusive]@ range.
--
--   /This is a specialization of 'integral', offered for convenience./
--
word64 :: Range Word64 -> Gen Word64
word64 =
  integral

------------------------------------------------------------------------
-- Combinators - Fractional / Floating-Point

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This generator works the same as 'integral', but for floating point numbers./
--
realFloat :: (RealFloat a) => Range a -> Gen a
realFloat range =
  shrink (Shrink.towardsFloat $ Range.origin range) (realFrac_ range)

-- | Generates a random fractional number in the [inclusive,exclusive) range.
--
--   /This generator does not shrink./
--
realFrac_ :: (RealFrac a) => Range a -> Gen a
realFrac_ range =
  generate $ \size seed ->
    let
      (x, y) =
        Range.bounds size range
    in
      realToFrac . fst $
        Seed.nextDouble (realToFrac x) (realToFrac y) seed

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This is a specialization of 'realFloat', offered for convenience./
--
float :: Range Float -> Gen Float
float =
 realFloat

-- | Generates a random floating-point number in the @[inclusive,exclusive)@ range.
--
--   /This is a specialization of 'realFloat', offered for convenience./
--
double :: Range Double -> Gen Double
double =
 realFloat

------------------------------------------------------------------------
-- Combinators - Enumeration

-- | Generates an element from an enumeration.
--
--   This generator shrinks towards the first argument.
--
--   For example:
--
-- @
-- enum \'a' \'z' :: 'Gen' 'Char'
-- @
--
enum :: (Enum a) => a -> a -> Gen a
enum lo hi =
  fmap toEnum . integral $
    Range.constant (fromEnum lo) (fromEnum hi)

-- | Generates a random value from a bounded enumeration.
--
--   This generator shrinks towards 'minBound'.
--
--   For example:
--
-- @
-- enumBounded :: 'Gen' 'Bool'
-- @
--
--   /This is implemented in terms of the 'Enum' class, and thus may be/
--   /partial for integral types larger than 'Int', e.g. 'Word64'./
enumBounded :: (Enum a, Bounded a) => Gen a
enumBounded =
  enum minBound maxBound

-- | Generates a random boolean.
--
--   This generator shrinks to 'False'.
--
--   /This is a specialization of 'enumBounded', offered for convenience./
--
bool :: Gen Bool
bool =
  enumBounded

-- | Generates a random boolean.
--
--   /This generator does not shrink./
--
bool_ :: Gen Bool
bool_ =
  generate $ \_ seed ->
    (/= 0) . fst $ Seed.nextInteger 0 1 seed

------------------------------------------------------------------------
-- Combinators - Characters

-- | Generates an ASCII binit: @'0'..'1'@
--
binit :: Gen Char
binit =
  enum '0' '1'

-- | Generates an ASCII octit: @'0'..'7'@
--
octit :: Gen Char
octit =
  enum '0' '7'

-- | Generates an ASCII digit: @'0'..'9'@
--
digit :: Gen Char
digit =
  enum '0' '9'

-- | Generates an ASCII hexit: @'0'..'9', \'a\'..\'f\', \'A\'..\'F\'@
--
hexit :: Gen Char
hexit =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "0123456789aAbBcCdDeEfF"

-- | Generates an ASCII lowercase letter: @\'a\'..\'z\'@
--
lower :: Gen Char
lower =
  enum 'a' 'z'

-- | Generates an ASCII uppercase letter: @\'A\'..\'Z\'@
--
upper :: Gen Char
upper =
  enum 'A' 'Z'

-- | Generates an ASCII letter: @\'a\'..\'z\', \'A\'..\'Z\'@
--
alpha :: Gen Char
alpha =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Generates an ASCII letter or digit: @\'a\'..\'z\', \'A\'..\'Z\', \'0\'..\'9\'@
--
alphaNum :: Gen Char
alphaNum =
  -- FIXME optimize lookup, use a SmallArray or something.
  element "abcdefghiklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"

-- | Generates an ASCII character: @'\0'..'\127'@
--
ascii :: Gen Char
ascii =
  enum '\0' '\127'

-- | Generates a Latin-1 character: @'\0'..'\255'@
--
latin1 :: Gen Char
latin1 =
  enum '\0' '\255'

-- | Generates a Unicode character, excluding noncharacters and invalid standalone surrogates:
--   @'\0'..'\1114111' (excluding '\55296'..'\57343', '\65534', '\65535')@
--
unicode :: Gen Char
unicode =
  let
    s1 =
      (55296, enum '\0' '\55295')
    s2 =
      (8190, enum '\57344' '\65533')
    s3 =
      (1048576, enum '\65536' '\1114111')
  in
    frequency [s1, s2, s3]

-- | Generates a Unicode character, including noncharacters and invalid standalone surrogates:
--   @'\0'..'\1114111'@
--
unicodeAll :: Gen Char
unicodeAll =
  enumBounded

------------------------------------------------------------------------
-- Combinators - Strings

-- | Generates a string using 'Range' to determine the length.
--
--   /This is a specialization of 'list', offered for convenience./
--
string :: Range Int -> Gen Char -> Gen String
string =
  list

-- | Generates a string using 'Range' to determine the length.
--
text :: Range Int -> Gen Char -> Gen Text
text range =
  fmap Text.pack . string range

-- | Generates a UTF-8 encoded string, using 'Range' to determine the length.
--
utf8 :: Range Int -> Gen Char -> Gen ByteString
utf8 range =
  fmap Text.encodeUtf8 . text range

-- | Generates a random 'ByteString', using 'Range' to determine the
--   length.
--
bytes :: Range Int -> Gen ByteString
bytes range =
  fmap ByteString.pack $
  choice [
      list range . word8 $
        Range.constant
          (fromIntegral $ Char.ord 'a')
          (fromIntegral $ Char.ord 'z')

    , list range . word8 $
        Range.constant minBound maxBound
    ]

------------------------------------------------------------------------
-- Combinators - Choice

-- | Trivial generator that always produces the same element.
--
--   /This is another name for 'pure' \/ 'return'./
constant :: a -> Gen a
constant =
  pure

-- | Randomly selects one of the elements in the list.
--
--   This generator shrinks towards the first element in the list.
--
--   /The input list must be non-empty./
--
element :: (Foldable f) => f a -> Gen a
element fa = case toList fa of
  [] ->
    error "Hedgehog.Gen.element: used with empty Foldable"
  xs -> do
    n <- integral $ Range.constant 0 (length xs - 1)
    pure $ xs !! n

-- | Randomly selects one of the elements in the list.
--
--   This generator does not shrink the choice of element.
--
--   /The input list must be non-empty./
--
element_ :: [a] -> Gen a
element_ = \case
  [] ->
    error "Hedgehog.Gen.element: used with empty list"
  xs -> do
    n <- integral_ $ Range.constant 0 (length xs - 1)
    pure $ xs !! n

-- | Randomly selects one of the generators in the list.
--
--   This generator shrinks towards the first generator in the list.
--
--   /The input list must be non-empty./
--
choice :: [Gen a] -> Gen a
choice = \case
  [] ->
    error "Hedgehog.Gen.choice: used with empty list"
  xs -> do
    n <- integral $ Range.constant 0 (length xs - 1)
    xs !! n

-- | Uses a weighted distribution to randomly select one of the generators in
--   the list.
--
--   This generator shrinks towards the first generator in the list.
--
--   /The input list must be non-empty./
--
frequency :: [(Int, Gen a)] -> Gen a
frequency = \case
  [] ->
    error "Hedgehog.Gen.frequency: used with empty list"
  xs0 -> do
    let
      pick n = \case
        [] ->
          error "Hedgehog.Gen.frequency/pick: used with empty list"
        (k, x) : xs ->
          if n <= k then
            x
          else
            pick (n - k) xs

      iis =
        scanl1 (+) (fmap fst xs0)

      total =
        sum (fmap fst xs0)

    n <- shrink (\n -> takeWhile (< n) iis) $ integral_ $ Range.constant 1 total
    pick n xs0

-- | Modifies combinators which choose from a list of generators, like 'choice'
--   or 'frequency', so that they can be used in recursive scenarios.
--
--   This combinator modifies its target to select one of the generators in
--   either the non-recursive or the recursive list. When a selection is made
--   from the recursive list, the 'Size' is halved. When the 'Size' gets to one
--   or less, selections are no longer made from the recursive list, this
--   ensures termination.
--
--   A good example of where this might be useful is abstract syntax trees:
--
-- @
-- data Expr =
--     Var String
--   | Lam String Expr
--   | App Expr Expr
--
-- -- Assuming we have a name generator
-- genName :: 'MonadGen' m => m String
--
-- -- We can write a generator for expressions
-- genExpr :: 'MonadGen' m => m Expr
-- genExpr =
--   Gen.'recursive' Gen.'choice' [
--       -- non-recursive generators
--       Var '<$>' genName
--     ] [
--       -- recursive generators
--       Gen.'subtermM' genExpr (\x -> Lam '<$>' genName '<*>' pure x)
--     , Gen.'subterm2' genExpr genExpr App
--     ]
-- @
--
--   If we wrote the above example using only 'choice', it is likely that it
--   would fail to terminate. This is because for every call to @genExpr@,
--   there is a 2 in 3 chance that we will recurse again.
--
recursive :: ([Gen a] -> Gen a) -> [Gen a] -> [Gen a] -> Gen a
recursive f nonrec rec =
  sized $ \n ->
    if n <= 1 then
      f nonrec
    else
      f $ nonrec ++ fmap small rec

------------------------------------------------------------------------
-- Combinators - Conditional

-- | Discards the whole generator.
--
discard :: Gen a
discard =
  empty

-- | Discards the generator if the generated value does not satisfy the
--   predicate.
--
ensure :: (a -> Bool) -> Gen a -> Gen a
ensure p gen = do
  x <- gen
  if p x then
    pure x
  else
    discard

fromPred :: (a -> Bool) -> a -> Maybe a
fromPred p a = a <$ guard (p a)

-- | Generates a value that satisfies a predicate.
--
--   Shrinks of the generated value will also satisfy the predicate. From the
--   original generator's shrink tree, any values that fail the predicate will
--   be removed, but any subsequent shrinks that satisfy it will be retained.
--   Compared to 'filter', shrinking may be slower but will be optimal.
--
--   It's possible that the predicate will never pass, or will only pass at a
--   larger size than we're currently running at. To avoid looping forever, we
--   limit the number of retries, and grow the size with each retry. If we retry
--   too many times then the whole generator is discarded.
--
filter :: (a -> Bool) -> Gen a -> Gen a
filter p =
  mapMaybe (fromPred p)

-- | Generates a value which is the result of the given function returning a
--   'Just'.
--
--   The original generator's shrink tree will be retained, with values
--   returning 'Nothing' removed. Subsequent shrinks of those values will be
--   retained. Compared to 'mapMaybeT', shrinking may be slower but will be
--   optimal.
--
--   It's possible that the function will never return 'Just', or will only do
--   so a larger size than we're currently running at. To avoid looping forever,
--   we limit the number of retries, and grow the size with each retry. If we
--   retry too many times then the whole generator is discarded.
--
mapMaybe :: (a -> Maybe b) -> Gen a -> Gen b
mapMaybe p gen0 =
  let
    try k =
      if k > 100 then
        discard
      else do
        (x, gen) <- freeze $ scale (2 * k +) gen0

        case p x of
          Just _ ->
            mapGen (Tree.mapMaybeMaybeT p) gen
          Nothing ->
            try (k + 1)
  in
    try 0

-- | Generates a value that satisfies a predicate.
--
--   Shrinks of the generated value will also satisfy the predicate. From the
--   original generator's shrink tree, any values that fail the predicate will
--   be removed, along with their subsequent shrinks. Compared to 'filter',
--   shrinking may be faster but may also be less optimal.
--
--   The type is also more general, because the shrink behavior from 'filter'
--   would force the entire shrink tree to be evaluated when applied to an
--   impure tree.
--
--   This is essentially:
--
-- @
--   filterT p gen = 'mfilter' p gen '<|>' filterT p gen
-- @
--
--   But that could loop forever, if the predicate will never pass or will only
--   pass at a larger size than we're currently running at. We differ from the
--   above in keeping some state to avoid that. We limit the number of retries,
--   and grow the size with each retry. If we retry too many times then the
--   whole generator is discarded.
--
filterT :: (a -> Bool) -> Gen a -> Gen a
filterT p =
  mapMaybeT (fromPred p)

-- | Generates a value which is the result of the given function returning a
--   'Just'.
--
--   The original generator's shrink tree will be retained, with values
--   returning 'Nothing' removed. Subsequent shrinks of those values will be
--   retained. Compared to 'mapMaybeT', shrinking may be slower but will be
--   optimal.
--
--   The type is also more general, because the shrink behavior from 'mapMaybe'
--   would force the entire shrink tree to be evaluated when applied to an
--   impure tree.
--
--   It's possible that the function will never return 'Just', or will only do
--   so a larger size than we're currently running at. To avoid looping forever,
--   we limit the number of retries, and grow the size with each retry. If we
--   retry too many times then the whole generator is discarded.
--
mapMaybeT :: (a -> Maybe b) -> Gen a -> Gen b
mapMaybeT p gen0 =
  let
    try k =
      if k > 100 then
        discard
      else do
        (x, gen) <- freeze $ scale (2 * k +) gen0

        case p x of
          Just _ ->
            mapGen (Tree.mapMaybeT p) gen
          Nothing ->
            try (k + 1)
  in
    try 0

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   /This is implemented using 'filter' and has the same caveats./
--
just :: Gen (Maybe a) -> Gen a
just g = do
  mx <- filter Maybe.isJust g
  case mx of
    Just x ->
      pure x
    Nothing ->
      error "Hedgehog.Gen.just: internal error, unexpected Nothing"

-- | Runs a 'Maybe' generator until it produces a 'Just'.
--
--   /This is implemented using 'filter' and has the same caveats./
--
justT :: Gen (Maybe a) -> Gen a
justT g = do
  mx <- filterT Maybe.isJust g
  case mx of
    Just x ->
      pure x
    Nothing ->
      error "Hedgehog.Gen.just: internal error, unexpected Nothing"

------------------------------------------------------------------------
-- Combinators - Collections

-- | Generates a 'Nothing' some of the time.
--
maybe :: Gen a -> Gen (Maybe a)
maybe gen =
  sized $ \n ->
    frequency [
        (2, pure Nothing)
      , (1 + fromIntegral n, Just <$> gen)
      ]

-- | Generates either an 'a' or a 'b'.
--
--   As the size grows, this generator generates @Right@s more often than @Left@s.
--
either :: Gen a -> Gen b -> Gen (Either a b)
either genA genB =
  sized $ \n ->
    frequency [
        (2, Left <$> genA)
      , (1 + fromIntegral n, Right <$> genB)
      ]

-- | Generates either an 'a' or a 'b', without bias.
--
--   This generator generates as many @Right@s as it does @Left@s.
--
either_ :: Gen a -> Gen b -> Gen (Either a b)
either_ genA genB =
    choice [
      Left <$> genA
    , Right <$> genB
    ]

-- | Generates a list using a 'Range' to determine the length.
--
list :: Range Int -> Gen a -> Gen [a]
list range gen =
  let
     interleave =
       (Tree.interleaveTreeT . nodeValue =<<)
  in
    sized $ \size ->
      ensure (atLeast $ Range.lowerBound size range) .
      mapGen (TreeT . interleave . runTreeT) $ do
        n <- integral_ range
        replicateM n (toTreeMaybeT gen)

-- | Generates a seq using a 'Range' to determine the length.
--
seq :: Range Int -> Gen a -> Gen (Seq a)
seq range gen =
  Seq.fromList <$> list range gen

-- | Generates a non-empty list using a 'Range' to determine the length.
--
nonEmpty :: Range Int -> Gen a -> Gen (NonEmpty a)
nonEmpty range gen = do
  xs <- list (fmap (max 1) range) gen
  case xs of
    [] ->
      error "Hedgehog.Gen.nonEmpty: internal error, generated empty list"
    _ ->
      pure $ NonEmpty.fromList xs

-- | Generates a set using a 'Range' to determine the length.
--
--   /This may fail to generate anything if the element generator/
--   /cannot produce a large enough number of unique items to satify/
--   /the required set size./
--
set :: (Ord a) => Range Int -> Gen a -> Gen (Set a)
set range gen =
  fmap Map.keysSet . map range $ fmap (, ()) gen

-- | Generates a map using a 'Range' to determine the length.
--
--   /This may fail to generate anything if the keys produced by the/
--   /generator do not account for a large enough number of unique/
--   /items to satify the required map size./
--
map :: (Ord k) => Range Int -> Gen (k, v) -> Gen (Map k v)
map range gen =
  sized $ \size ->
    ensure ((>= Range.lowerBound size range) . Map.size) .
    fmap Map.fromList .
    (sequence =<<) .
    shrink Shrink.list $ do
      k <- integral_ range
      uniqueByKey k gen

-- | Generate exactly 'n' unique generators.
--
uniqueByKey :: (Ord k) => Int -> Gen (k, v) -> Gen [Gen (k, v)]
uniqueByKey n gen =
  let
    try k xs0 =
      if k > 100 then
        discard
      else
        replicateM n (freeze gen) >>= \kvs ->
        case uniqueInsert n xs0 (fmap (first fst) kvs) of
          Left xs ->
            pure $ Map.elems xs
          Right xs ->
            try (k + 1) xs
  in
    try (0 :: Int) Map.empty

uniqueInsert :: Ord k => Int -> Map k v -> [(k, v)] -> Either (Map k v) (Map k v)
uniqueInsert n xs kvs0 =
  if Map.size xs >= n then
    Left xs
  else
    case kvs0 of
      [] ->
        Right xs
      (k, v) : kvs ->
        uniqueInsert n (Map.insertWith (\x _ -> x) k v xs) kvs

-- | Check that list contains at least a certain number of elements.
--
atLeast :: Int -> [a] -> Bool
atLeast n =
  if n == 0 then
    const True
  else
    not . null . drop (n - 1)

------------------------------------------------------------------------
-- Combinators - Subterms

data Subterms n a =
    One a
  | All (Vec n a)
    deriving (Functor, Foldable, Traversable)

data Nat =
    Z
  | S Nat

data Vec n a where
  Nil :: Vec 'Z a
  (:.) :: a -> Vec n a -> Vec ('S n) a

infixr 5 :.

deriving instance Functor (Vec n)
deriving instance Foldable (Vec n)
deriving instance Traversable (Vec n)

-- | Freeze the size and seed used by a generator, so we can inspect the value
--   which it will produce.
--
--   This is used for implementing `list` and `subtermMVec`. It allows us to
--   shrink the list itself before trying to shrink the values inside the list.
--
freeze :: Gen a -> Gen (a, Gen a)
freeze gen =
  Gen $ \size seed ->
    case runTreeT (runGen size seed gen) of
      Nothing ->
        empty
      Just (NodeT x xs) ->
        pure (x, fromTreeMaybeT . Tree.fromNodeT $ NodeT x xs)

shrinkSubterms :: Subterms n a -> [Subterms n a]
shrinkSubterms = \case
  One _ ->
    []
  All xs ->
    fmap One $ toList xs

genSubterms :: Vec n (Gen a) -> Gen (Subterms n a)
genSubterms =
  (sequence =<<) .
  shrink shrinkSubterms .
  fmap All .
  mapM (fmap snd . freeze)

fromSubterms :: Applicative m => (Vec n a -> m a) -> Subterms n a -> m a
fromSubterms f = \case
  One x ->
    pure x
  All xs ->
    f xs

-- | Constructs a generator from a number of sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermMVec :: Vec n (Gen a) -> (Vec n a -> Gen a) -> Gen a
subtermMVec gs f =
  fromSubterms f =<< genSubterms gs

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subtermM :: Gen a -> (a -> Gen a) -> Gen a
subtermM gx f =
  subtermMVec (gx :. Nil) $ \(x :. Nil) ->
    f x

-- | Constructs a generator from a sub-term generator.
--
--   /Shrinks to the sub-term if possible./
--
subterm :: Gen a -> (a -> a) -> Gen a
subterm gx f =
  subtermM gx $ \x ->
    pure (f x)

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM2 :: Gen a -> Gen a -> (a -> a -> Gen a) -> Gen a
subtermM2 gx gy f =
  subtermMVec (gx :. gy :. Nil) $ \(x :. y :. Nil) ->
    f x y

-- | Constructs a generator from two sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm2 :: Gen a -> Gen a -> (a -> a -> a) -> Gen a
subterm2 gx gy f =
  subtermM2 gx gy $ \x y ->
    pure (f x y)

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subtermM3 :: Gen a -> Gen a -> Gen a -> (a -> a -> a -> Gen a) -> Gen a
subtermM3 gx gy gz f =
  subtermMVec (gx :. gy :. gz :. Nil) $ \(x :. y :. z :. Nil) ->
    f x y z

-- | Constructs a generator from three sub-term generators.
--
--   /Shrinks to one of the sub-terms if possible./
--
subterm3 :: Gen a -> Gen a -> Gen a -> (a -> a -> a -> a) -> Gen a
subterm3 gx gy gz f =
  subtermM3 gx gy gz $ \x y z ->
    pure (f x y z)

------------------------------------------------------------------------
-- Combinators - Combinations & Permutations

-- | Generates a random subsequence of a list.
--
-- For example:
--
-- @
-- Gen.print (Gen.subsequence [1..5])
-- @
--
--   > === Outcome ===
--   > [1,2,4]
--   > === Shrinks ===
--   > []
--   > [2,4]
--   > [1,4]
--   > [1,2]
--
subsequence :: [a] -> Gen [a]
subsequence xs =
  shrink Shrink.list $ filterM (const bool_) xs

-- | Generates a random subset of a set.
--
--  /This shrinks towards the empty set./
--
subset :: Set a -> Gen (Set a)
-- Set.fromDistinctAscList has an unchecked precondition that the list
-- must be strictly ascending. This precondition is satisfied because
-- Set.toAscList produces a strictly ascending list, and the 'subsequence'
-- generator only removes elements from the list; it never adds or
-- rearranges elements, so the strictly ascending property is undisturbed.
subset =
  fmap Set.fromDistinctAscList . subsequence . Set.toAscList

-- | Generates a random permutation of a list.
--
--   /This shrinks towards the order of the list being identical to the input/
--   /list./
--
shuffle :: [a] -> Gen [a]
-- We shuffle sequences instead of lists to make extracting an arbitrary
-- element logarithmic instead of linear, and to make length calculation
-- constant-time instead of linear. We could probably do better, but
-- this is at least reasonably quick.
shuffle = fmap toList . shuffleSeq . Seq.fromList

-- | Generates a random permutation of a sequence.
--
--   /This shrinks towards the order of the sequence being identical to the input/
--   /sequence./
--
shuffleSeq :: Seq a -> Gen (Seq a)
shuffleSeq xs =
  if null xs then
    pure Seq.empty
  else do
    n <- integral $ Range.constant 0 (length xs - 1)
#if MIN_VERSION_containers(0,5,8)
    -- Data.Sequence should offer a version of deleteAt that returns the
    -- deleted element, but it does not currently do so. Lookup followed
    -- by deletion seems likely faster than splitting and then appending,
    -- but I haven't actually tested that. It's certainly easier to see
    -- what's going on.
    case Seq.lookup n xs of
      Just y ->
        (y Seq.<|) <$> shuffleSeq (Seq.deleteAt n xs)
      Nothing ->
        error "Hedgehog.Gen.shuffleSeq: internal error, lookup in empty sequence"
#else
    case Seq.splitAt n xs of
      (beginning, end) ->
        case Seq.viewl end of
          y Seq.:< end' ->
            (y Seq.<|) <$> shuffleSeq (beginning Seq.>< end')
          Seq.EmptyL ->
            error "Hedgehog.Gen.shuffleSeq: internal error, lookup in empty sequence"
#endif

------------------------------------------------------------------------
-- Sampling

-- | Generate a sample from a generator.
--
-- This function is useful for examining a 'Gen' in GHCi or other contexts.
-- It is not appropriate for use in a test suite directly. You will only
-- get a single sample from this function, and it will not give you
-- a property test. The seed is random, so the test is not deterministic.
--
-- If you only want a single test to run, then use @'withTests' 1@:
--
-- @
-- prop_OnlyRunOnce :: Property
-- prop_OnlyRunOnce =
--   'withTests' 1 $ 'property' $ do
--     i <- Gen.int
--     i /== 0
-- @
sample :: MonadIO m => Gen a -> m a
sample gen =
  liftIO $
    let
      loop n =
        if n <= 0 then
          error "Hedgehog.Gen.sample: too many discards, could not generate a sample"
        else do
          seed <- Seed.random
          case evalGen 30 seed gen of
            Nothing ->
              loop (n - 1)
            Just x ->
              pure $ Tree.treeValue x
    in
      loop (100 :: Int)

-- | Run a generator with a random seed and print the outcome, and the first
--   level of shrinks.
--
-- @
-- Gen.print (Gen.'enum' \'a\' \'f\')
-- @
--
--   > === Outcome ===
--   > 'd'
--   > === Shrinks ===
--   > 'a'
--   > 'b'
--   > 'c'
--
print :: (MonadIO m, Show a) => Gen a -> m ()
print gen = do
  seed <- liftIO Seed.random
  printWith 30 seed gen

-- | Print the value produced by a generator, and the first level of shrinks,
--   for the given size and seed.
--
--   Use 'print' to generate a value from a random seed.
--
printWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printWith size seed gen =
  liftIO $ do
    case evalGen size seed gen of
      Nothing -> do
        putStrLn "=== Outcome ==="
        putStrLn "<discard>"

      Just tree_ -> do
        let
          NodeT x ss =
            runIdentity (runTreeT tree_)

        putStrLn "=== Outcome ==="
        putStrLn (show x)
        putStrLn "=== Shrinks ==="

        for_ ss $ \s ->
          let
            NodeT y _ =
              runIdentity $ runTreeT s
          in
            putStrLn (show y)

-- | Run a generator with a random seed and print the resulting shrink tree.
--
-- @
-- Gen.printTree (Gen.'enum' \'a\' \'f\')
-- @
--
--   > 'd'
--   >  ├╼'a'
--   >  ├╼'b'
--   >  │  └╼'a'
--   >  └╼'c'
--   >     ├╼'a'
--   >     └╼'b'
--   >        └╼'a'
--
--   /This may not terminate when the tree is very large./
--
printTree :: (MonadIO m, Show a) => Gen a -> m ()
printTree gen = do
  seed <- liftIO Seed.random
  printTreeWith 30 seed gen

-- | Print the shrink tree produced by a generator, for the given size and
--   seed.
--
--   Use 'printTree' to generate a value from a random seed.
--
printTreeWith :: (MonadIO m, Show a) => Size -> Seed -> Gen a -> m ()
printTreeWith size seed gen = do
  liftIO . putStr $
    renderTree size seed gen

-- | Render the shrink tree produced by a generator, for the given size and
--   seed.
--
renderTree :: Show a => Size -> Seed -> Gen a -> String
renderTree size seed gen =
  case evalGen size seed gen of
    Nothing ->
      "<discard>"
    Just x ->
      Tree.render (fmap show x)

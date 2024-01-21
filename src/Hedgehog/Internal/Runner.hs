{-# LANGUAGE CPP #-}
{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Runner
  ( -- * Running Individual Properties
    check,
    recheck,
    recheckAt,

    -- * Running Groups of Properties
    checkGroup,
  )
where

import Control.Concurrent.STM (TVar, atomically)
import Control.Concurrent.STM.TVar qualified as TVar
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Text qualified as Text
import Hedgehog.Internal.Config
import Hedgehog.Internal.Gen (runGen)
import Hedgehog.Internal.Property (CoverCount (..), Coverage (..), DiscardCount (..), Failure (..), Group (..), Journal (..), Property (..), PropertyConfig (..), PropertyCount (..), ShrinkCount (..), ShrinkPath (..), Test, TestCount (..), coverageSuccess, journalCoverage, runTest, withSkip, withTests)
import Hedgehog.Internal.Queue
import Hedgehog.Internal.Range (Size)
import Hedgehog.Internal.Region
import Hedgehog.Internal.Report
import Hedgehog.Internal.Seed qualified as Seed
import Hedgehog.Internal.Tree (Tree (..))
import System.IO (hSetEncoding, stderr, stdout, utf8)
import Prelude

-- | Configuration for a property test run.
data RunnerConfig = RunnerConfig
  { -- | The number of property tests to run concurrently. 'Nothing' means
    --   use one worker per processor.
    runnerWorkers :: !(Maybe WorkerCount),
    -- | Whether to use colored output or not. 'Nothing' means detect from
    --   the environment.
    runnerColor :: !(Maybe UseColor),
    -- | The seed to use. 'Nothing' means detect from the environment.
    runnerSeed :: !(Maybe Seed),
    -- | How verbose to be in the runner output. 'Nothing' means detect from
    --   the environment.
    runnerVerbosity :: !(Maybe Verbosity)
  }
  deriving (Eq, Ord, Show)

findM :: (Monad m) => [a] -> b -> (a -> m (Maybe b)) -> m b
findM xs0 def p =
  case xs0 of
    [] -> return def
    x0 : xs ->
      p x0 >>= \case
        Nothing -> findM xs def p
        Just x -> return x

isFailure :: Tree (Either x a, b) -> Bool
isFailure = \case
  Tree (Left _, _) _ -> True
  _ -> False

takeSmallest ::
  ShrinkCount ->
  ShrinkPath ->
  Int -> -- shrink limit
  (Progress -> IO ()) ->
  Maybe (Tree (Either Failure (), Journal)) ->
  IO Result
takeSmallest shrinks0 (ShrinkPath shrinkPath0) shrinkLimit updateUI =
  let loop shrinks revShrinkPath = \case
        Tree (x, (Journal logs)) xs ->
          case x of
            Left (Failure loc err mdiff) -> do
              let shrinkPath = ShrinkPath $ reverse revShrinkPath
                  failure = mkFailure shrinks shrinkPath Nothing loc err mdiff (reverse logs)

              updateUI $ Shrinking failure

              if shrinks >= fromIntegral shrinkLimit
                then -- if we've hit the shrink limit, don't shrink any further
                  pure $ Failed failure
                else findM (zip [0 ..] xs) (Failed failure) $ \(n, m) ->
                  if isFailure m
                    then Just <$> loop (shrinks + 1) (n : revShrinkPath) m
                    else return Nothing
            Right () ->
              return OK
   in maybe (pure GaveUp) (loop shrinks0 (reverse shrinkPath0))

-- | Follow a given shrink path, instead of searching exhaustively. Assume that
-- the end of the path is minimal, and don't try to shrink any further than
-- that.
--
-- This evaluates the test for all the shrinks on the path, but not ones
-- off-path. Because the generator is mixed with the test code, it's probably
-- not possible to avoid this.
skipToShrink ::
  ShrinkPath ->
  (Progress -> IO ()) ->
  Maybe (Tree (Either Failure (), Journal)) ->
  IO Result
skipToShrink (ShrinkPath shrinkPath) updateUI =
  let loop shrinks [] = \case
        Tree (x, (Journal logs)) _ ->
          case x of
            Left (Failure loc err mdiff) -> do
              let failure = mkFailure shrinks (ShrinkPath shrinkPath) Nothing loc err mdiff (reverse logs)
              updateUI $ Shrinking failure
              pure $ Failed failure
            Right () -> return OK
      loop shrinks (s0 : ss) = \case
        Tree _ xs ->
          case drop s0 xs of
            [] -> pure GaveUp
            (x : _) -> do loop (shrinks + 1) ss x
   in maybe (pure GaveUp) (loop 0 shrinkPath)

checkReport ::
  PropertyConfig ->
  Size ->
  Seed ->
  Test () ->
  (Report Progress -> IO ()) ->
  IO (Report Result)
checkReport cfg size0 seed0 test updateUI = do
  skip <- resolveSkip $ propertySkip cfg

  let (mSkipToTest, mSkipToShrink) =
        case skip of
          SkipNothing -> (Nothing, Nothing)
          SkipToTest t d -> (Just (t, d), Nothing)
          SkipToShrink t d s -> (Just (t, d), Just s)

      testLimit = propertyTestLimit cfg

      loop ::
        TestCount ->
        DiscardCount ->
        Size ->
        Seed ->
        Coverage CoverCount ->
        IO (Report Result)
      loop !tests !discards !size !seed !coverage0 = do
        updateUI $ Report tests discards coverage0 seed0 Running

        let enoughTestsRun = tests >= TestCount testLimit
            labelsCovered = coverageSuccess tests coverage0
            successReport = Report tests discards coverage0 seed0 OK

            failureReport message =
              Report tests discards coverage0 seed0 . Failed $
                mkFailure
                  0
                  (ShrinkPath [])
                  (Just coverage0)
                  Nothing
                  message
                  Nothing
                  []

        if
          -- size has reached limit, reset to 0
          | size > 99 -> loop tests discards 0 seed coverage0
          -- at this point, we know that enough tests have been run in order to
          -- make a decision on if this was a successful run or not
          --
          -- If we have early termination, then we need to check coverageReached /
          -- coverageUnreachable. If we skip tests, we ignore coverage.
          | enoughTestsRun ->
              pure
                if isJust mSkipToTest
                  then successReport
                  else
                    if labelsCovered
                      then successReport
                      else
                        failureReport $
                          "Labels not sufficently covered after " <> show tests <> " tests"
          -- we've hit the discard limit, give up
          | discards >= fromIntegral (propertyDiscardLimit cfg) ->
              pure $ Report tests discards coverage0 seed0 GaveUp
          | otherwise ->
              case Seed.split seed of
                (s0, s1) -> case (mSkipToTest, mSkipToShrink) of
                  -- If the report says failed "after 32 tests", the test number that
                  -- failed was 31, but we want the user to be able to skip to 32 and
                  -- start with the one that failed.
                  (Just (n, d), _)
                    | n > tests + 1 -> loop (tests + 1) discards (size + 1) s1 coverage0
                    | d > discards -> loop tests (discards + 1) (size + 1) s1 coverage0
                  (Just _, Just shrinkPath) -> do
                    let node = runGen size s0 . runTest $ test
                    let mkReport :: a -> Report a
                        mkReport = Report (tests + 1) discards coverage0 seed0
                    mkReport <$> skipToShrink shrinkPath (updateUI . mkReport) node
                  _ -> do
                    let node = runGen size s0 . runTest $ test
                    case node of
                      Nothing -> loop tests (discards + 1) (size + 1) s1 coverage0
                      Just (Tree (Left _, _) _) ->
                        let mkReport :: a -> Report a
                            mkReport =
                              Report (tests + 1) discards coverage0 seed0
                         in fmap mkReport $
                              takeSmallest
                                0
                                (ShrinkPath [])
                                (propertyShrinkLimit cfg)
                                (updateUI . mkReport)
                                node
                      Just (Tree (Right (), journal) _) ->
                        let coverage =
                              journalCoverage journal <> coverage0
                         in loop (tests + 1) discards (size + 1) s1 coverage

  loop 0 0 size0 seed0 mempty

checkRegion ::
  Region ->
  UseColor ->
  Maybe Text ->
  Size ->
  Seed ->
  Property ->
  IO (Report Result)
checkRegion region color name size seed prop = do
  result <-
    checkReport (propertyConfig prop) size seed (propertyTest prop) \progress -> do
      ppprogress <- renderProgress color name progress
      atomically case reportStatus progress of
        Running -> setRegion region ppprogress
        Shrinking _ -> openRegion region ppprogress

  ppresult <- renderResult color name result
  atomically case reportStatus result of
    Failed _ -> openRegion region ppresult
    GaveUp -> openRegion region ppresult
    OK -> setRegion region ppresult

  pure result

checkNamed ::
  Region ->
  UseColor ->
  Maybe Text ->
  Maybe Seed ->
  Property ->
  IO (Report Result)
checkNamed region color name mseed prop = do
  seed <- resolveSeed mseed
  checkRegion region color name 0 seed prop

-- | Check a property.
check :: Property -> IO Bool
check prop = do
  color <- detectColor
  displayRegion \region ->
    (== OK) . reportStatus <$> checkNamed region color Nothing Nothing prop

-- | Check a property using a specific size and seed.
recheck :: Size -> Seed -> Property -> IO ()
recheck size seed prop0 = do
  color <- detectColor
  let prop = withTests 1 prop0
  _ <- displayRegion \region ->
    checkRegion region color Nothing size seed prop
  pure ()

recheckAt :: Seed -> Skip -> Property -> IO ()
recheckAt seed skip prop0 = do
  color <- detectColor
  let prop = withSkip skip prop0
  _ <- displayRegion \region ->
    checkRegion region color Nothing 0 seed prop
  pure ()

-- | Check a group of properties.
checkGroup :: Group -> IO Bool
checkGroup (Group group props) = do
  n <- detectWorkers

  -- ensure few spare capabilities for concurrent-output, it's likely that
  -- our tests will saturate all the capabilities they're given.
  updateNumCapabilities (n + 2)

  hSetEncoding stdout utf8
  hSetEncoding stderr utf8

  putStrLn $ "━━━ " ++ Text.unpack group ++ " ━━━"

  seed <- detectSeed
  verbosity <- detectVerbosity
  color <- detectColor

  displayRegion $ \sregion -> do
    svar <- atomically . TVar.newTVar $ mempty {summaryWaiting = PropertyCount (length props)}

    let start (TasksRemaining tasks) _ix (name, prop) = do
          updateSummary sregion svar color \x ->
            x
              { summaryWaiting = PropertyCount tasks,
                summaryRunning = summaryRunning x + 1
              }

          atomically do
            region <-
              case verbosity of
                Quiet -> newEmptyRegion
                Normal -> newOpenRegion

            moveToBottom sregion

            pure (name, prop, region)

        finish (_name, _prop, _region) =
          updateSummary sregion svar color \x ->
            x {summaryRunning = summaryRunning x - 1}

        finalize (_name, _prop, region) =
          finishRegion region

    summary <-
      fmap (mconcat . fmap (fromResult . reportStatus)) $
        runTasks n props start finish finalize \(name, prop, region) -> do
          result <- checkNamed region color (Just name) (Just seed) prop
          updateSummary sregion svar color (<> fromResult (reportStatus result))
          pure result

    updateSummary sregion svar color (const summary)

    pure $
      summaryFailed summary == 0
        && summaryGaveUp summary == 0

updateSummary :: Region -> TVar Summary -> UseColor -> (Summary -> Summary) -> IO ()
updateSummary sregion svar color f = do
  summary <- atomically (TVar.modifyTVar' svar f >> TVar.readTVar svar)
  atomically . setRegion sregion =<< renderSummary color summary

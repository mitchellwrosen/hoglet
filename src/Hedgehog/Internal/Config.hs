{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Config
  ( UseColor (..),
    Seed (..),
    resolveSeed,
    Verbosity (..),
    WorkerCount (..),
    Skip (..),
    resolveSkip,
    detectColor,
    detectSeed,
    detectWorkers,
    detectVerbosity,
  )
where

import Data.Functor ((<&>))
import Data.Text qualified as Text
import GHC.Conc qualified as Conc
import Hedgehog.Internal.Property (Skip (..), skipDecompress)
import Hedgehog.Internal.Seed (Seed (..))
import Hedgehog.Internal.Seed qualified as Seed
import System.Console.ANSI (hSupportsANSI)
import System.Environment (lookupEnv)
import System.IO (stdout)
import Text.Read (readMaybe)

-- | Whether to render output using ANSI colors or not.
data UseColor
  = -- | Disable ANSI colors in report output.
    DisableColor
  | -- | Enable ANSI colors in report output.
    EnableColor
  deriving (Eq, Ord, Show)

-- | How verbose should the report output be.
data Verbosity
  = -- | Only display the summary of the test run.
    Quiet
  | -- | Display each property as it is running, as well as the summary.
    Normal
  deriving (Eq, Ord, Show)

-- | The number of workers to use when running properties in parallel.
newtype WorkerCount
  = WorkerCount Int
  deriving (Eq, Ord, Show, Num, Enum, Real, Integral)

detectMark :: IO Bool
detectMark =
  (== Just "mth") <$> lookupEnv "USER"

lookupBool :: String -> IO (Maybe Bool)
lookupBool key = do
  lookupEnv key <&> \case
    Just "0" -> Just False
    Just "no" -> Just False
    Just "false" -> Just False
    Just "1" -> Just True
    Just "yes" -> Just True
    Just "true" -> Just True
    _ -> Nothing

detectColor :: IO UseColor
detectColor =
  lookupBool "HEDGEHOG_COLOR" >>= \case
    Just False -> pure DisableColor
    Just True -> pure EnableColor
    Nothing -> do
      mth <- detectMark
      if mth
        then pure DisableColor -- avoid getting fired :)
        else
          hSupportsANSI stdout <&> \case
            True -> EnableColor
            False -> DisableColor

splitOn :: String -> String -> [String]
splitOn needle haystack =
  fmap Text.unpack $ Text.splitOn (Text.pack needle) (Text.pack haystack)

parseSeed :: String -> Maybe Seed
parseSeed env =
  case splitOn " " env of
    [value, gamma] -> Seed <$> readMaybe value <*> readMaybe gamma
    _ -> Nothing

detectSeed :: IO Seed
detectSeed = do
  menv <- lookupEnv "HEDGEHOG_SEED"
  case parseSeed =<< menv of
    Nothing -> Seed.random
    Just seed -> pure seed

detectVerbosity :: IO Verbosity
detectVerbosity = do
  menv <- (readMaybe @Int =<<) <$> lookupEnv "HEDGEHOG_VERBOSITY"
  case menv of
    Just 0 -> pure Quiet
    Just 1 -> pure Normal
    _ ->
      detectMark <&> \case
        True -> Quiet
        False -> Normal

detectWorkers :: IO WorkerCount
detectWorkers = do
  menv <- (readMaybe =<<) <$> lookupEnv "HEDGEHOG_WORKERS"
  case menv of
    Nothing -> WorkerCount <$> Conc.getNumProcessors
    Just env -> pure (WorkerCount env)

detectSkip :: IO Skip
detectSkip = do
  menv <- lookupEnv "HEDGEHOG_SKIP"
  case menv of
    Nothing -> pure SkipNothing
    Just env ->
      case skipDecompress env of
        -- It's clearer for the user if we error out here, rather than silently defaulting to SkipNothing.
        Nothing -> error "HEDGEHOG_SKIP is not a valid Skip."
        Just skip -> pure skip

resolveSeed :: Maybe Seed -> IO Seed
resolveSeed = \case
  Nothing -> detectSeed
  Just x -> pure x

resolveSkip :: Maybe Skip -> IO Skip
resolveSkip = \case
  Nothing -> detectSkip
  Just x -> pure x

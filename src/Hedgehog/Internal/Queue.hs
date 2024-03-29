{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Queue
  ( TasksRemaining (..),
    runTasks,
    updateNumCapabilities,
  )
where

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Concurrent.Async (forConcurrently)
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Concurrent.STM (STM)
import Control.Monad (when)
import Control.Monad.STM (atomically)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import GHC.Conc qualified as Conc
import Hedgehog.Internal.Config

newtype TaskIndex
  = TaskIndex Int
  deriving (Eq, Ord, Enum, Num)

newtype TasksRemaining
  = TasksRemaining Int

dequeueMVar ::
  MVar [(TaskIndex, a)] ->
  (TasksRemaining -> TaskIndex -> a -> IO b) ->
  IO (Maybe (TaskIndex, b))
dequeueMVar mvar start =
  MVar.modifyMVar mvar $ \case
    [] -> pure ([], Nothing)
    (ix, x) : xs -> do
      y <- start (TasksRemaining $ length xs) ix x
      pure (xs, Just (ix, y))

runTasks ::
  WorkerCount ->
  [a] ->
  (TasksRemaining -> TaskIndex -> a -> IO b) ->
  (b -> IO ()) ->
  (b -> STM ()) ->
  (b -> IO c) ->
  IO [c]
runTasks n tasks start finish finalize runTask = do
  qvar <- MVar.newMVar (zip [0 ..] tasks)
  fvar <- MVar.newMVar (-1, Map.empty)

  let worker rs = do
        mx <- dequeueMVar qvar start
        case mx of
          Nothing ->
            pure rs
          Just (ix, x) -> do
            r <- runTask x
            finish x
            finalizeTask fvar ix (finalize x)
            worker (r : rs)

  -- FIXME ensure all workers have finished running
  fmap concat . forConcurrently [1 .. max 1 n] $ \_ix ->
    worker []

runActiveFinalizers :: MVar (TaskIndex, Map TaskIndex (STM ())) -> IO ()
runActiveFinalizers mvar = do
  again <-
    MVar.modifyMVar mvar $ \original@(minIx, finalizers0) ->
      case Map.minViewWithKey finalizers0 of
        Nothing -> pure (original, False)
        Just ((ix, finalize), finalizers) ->
          if ix == minIx + 1
            then do
              atomically finalize
              pure ((ix, finalizers), True)
            else pure (original, False)

  when again $ runActiveFinalizers mvar

finalizeTask ::
  MVar (TaskIndex, Map TaskIndex (STM ())) ->
  TaskIndex ->
  STM () ->
  IO ()
finalizeTask mvar ix finalize = do
  MVar.modifyMVar_ mvar $ \(minIx, finalizers) ->
    pure (minIx, Map.insert ix finalize finalizers)
  runActiveFinalizers mvar

-- | Update the number of capabilities but never set it lower than it already
--   is.
updateNumCapabilities :: WorkerCount -> IO ()
updateNumCapabilities (WorkerCount n) = when rtsSupportsBoundThreads $ do
  ncaps <- Conc.getNumCapabilities
  Conc.setNumCapabilities (max n ncaps)

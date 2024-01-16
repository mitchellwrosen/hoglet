{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Region
  ( Region (..),
    newEmptyRegion,
    newOpenRegion,
    openRegion,
    setRegion,
    displayRegion,
    moveToBottom,
    finishRegion,
  )
where

import Control.Concurrent.STM (STM, TVar)
import Control.Concurrent.STM.TMVar qualified as TMVar
import Control.Concurrent.STM.TVar qualified as TVar
import Control.Exception (bracket)
import System.Console.Regions (ConsoleRegion, LiftRegion (..), RegionLayout (..))
import System.Console.Regions qualified as Console

data Body
  = Empty
  | Open ConsoleRegion
  | Closed

newtype Region = Region
  { unRegion :: TVar Body
  }

newEmptyRegion :: (LiftRegion m) => m Region
newEmptyRegion =
  liftRegion do
    ref <- TVar.newTVar Empty
    pure (Region ref)

newOpenRegion :: (LiftRegion m) => m Region
newOpenRegion =
  liftRegion do
    region <- Console.openConsoleRegion Linear
    ref <- TVar.newTVar (Open region)
    pure (Region ref)

openRegion :: (LiftRegion m) => Region -> String -> m ()
openRegion (Region var) content =
  liftRegion do
    TVar.readTVar var >>= \case
      Empty -> do
        region <- Console.openConsoleRegion Linear
        TVar.writeTVar var $ Open region
        Console.setConsoleRegion region content
      Open region -> Console.setConsoleRegion region content
      Closed -> pure ()

setRegion :: (LiftRegion m) => Region -> String -> m ()
setRegion (Region var) content =
  liftRegion do
    TVar.readTVar var >>= \case
      Empty -> pure ()
      Open region -> Console.setConsoleRegion region content
      Closed -> pure ()

displayRegions :: IO a -> IO a
displayRegions io =
  Console.displayConsoleRegions io

displayRegion :: (Region -> IO a) -> IO a
displayRegion =
  displayRegions . bracket newOpenRegion finishRegion

moveToBottom :: Region -> STM ()
moveToBottom (Region var) =
  liftRegion do
    TVar.readTVar var >>= \case
      Empty -> pure ()
      Open region ->
        TMVar.tryTakeTMVar Console.regionList >>= \case
          Nothing -> pure ()
          Just xs -> TMVar.putTMVar Console.regionList (region : filter (/= region) xs)
      Closed -> pure ()

finishRegion :: (LiftRegion m) => Region -> m ()
finishRegion (Region var) =
  liftRegion do
    TVar.readTVar var >>= \case
      Empty -> TVar.writeTVar var Closed
      Open region -> do
        content <- Console.getConsoleRegion region
        Console.finishConsoleRegion region content
        TVar.writeTVar var Closed
      Closed -> pure ()

{-# OPTIONS_HADDOCK not-home #-}

module Hedgehog.Internal.Exception
  ( tryEvaluate,
  )
where

import Control.Exception (AsyncException, Exception (..), SomeException (..), catch, evaluate, throwIO)
import System.IO.Unsafe (unsafePerformIO)

tryAll :: IO a -> IO (Either SomeException a)
tryAll m =
  catch (fmap Right m) $ \exception ->
    case fromException exception :: Maybe AsyncException of
      Nothing -> pure (Left exception)
      Just _ -> throwIO exception

tryEvaluate :: a -> Either SomeException a
tryEvaluate x =
  unsafePerformIO (tryAll (evaluate x))

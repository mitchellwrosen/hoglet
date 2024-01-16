module Hedgehog.Main
  ( -- * Running tests
    defaultMain,
  )
where

import Control.Monad (when)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stderr, stdout)

-- | An entry point that can be used as a main function.
defaultMain :: [IO Bool] -> IO ()
defaultMain tests = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  success <- and <$> sequence tests
  when (not success) exitFailure

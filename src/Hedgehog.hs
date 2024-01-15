-- |
-- This module includes almost everything you need to get started writing
-- property tests with Hedgehog.
--
-- It is designed to be used alongside "Hedgehog.Gen" and "Hedgehog.Range",
-- which should be imported qualified. You also need to enable Template Haskell
-- so the Hedgehog test runner can find your properties.
--
-- > {-# LANGUAGE TemplateHaskell #-}
-- >
-- > import           Hedgehog
-- > import qualified Hedgehog.Gen as Gen
-- > import qualified Hedgehog.Range as Range
--
-- Once you have your imports set up, you can write a simple property:
--
-- > prop_reverse :: Property
-- > prop_reverse =
-- >   property $ do
-- >     xs <- forAll $ Gen.list (Range.linear 0 100) Gen.alpha
-- >     reverse (reverse xs) === xs
--
-- And add the Template Haskell splice which will discover your properties:
--
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $$(discover)
--
-- If you prefer to avoid macros, you can specify the group of properties to
-- run manually instead:
--
-- > {-# LANGUAGE OverloadedStrings #-}
-- >
-- > tests :: IO Bool
-- > tests =
-- >   checkParallel $ Group "Test.Example" [
-- >       ("prop_reverse", prop_reverse)
-- >     ]
--
-- You can then load the module in GHCi, and run it:
--
-- > λ tests
-- > ━━━ Test.Example ━━━
-- >   ✓ prop_reverse passed 100 tests.
--
module Hedgehog (
  -- * Properties
    Property
  , Group(..)
  , GroupName

  , property

  , forAll
  , forAllWith
  , discard

  , check
  , recheck
  , recheckAt

  , checkParallel
  , checkSequential

  , Confidence
  , verifiedTermination
  , withConfidence

  , withTests
  , TestLimit

  , withDiscards
  , DiscardLimit

  , withShrinks
  , ShrinkLimit

  , withSkip
  , Skip

  -- * Generating Test Data
  , Gen

  , Range
  , Size(..)
  , Seed(..)

  -- * Tests
  , Test
  , annotate
  , annotateShow
  , footnote
  , footnoteShow
  , success
  , failure
  , assert
  , diff
  , (===)
  , (/==)
  , tripping

  , eval
  , evalNF
  , evalEither
  , evalExceptT
  , evalMaybe

  -- * Coverage
  , LabelName
  , classify
  , cover
  , label
  , collect
  ) where


import           Hedgehog.Internal.Gen (Gen)
import           Hedgehog.Internal.Property (annotate, annotateShow)
import           Hedgehog.Internal.Property (assert, diff, (===), (/==))
import           Hedgehog.Internal.Property (classify, cover)
import           Hedgehog.Internal.Property (discard, failure, success)
import           Hedgehog.Internal.Property (DiscardLimit, withDiscards)
import           Hedgehog.Internal.Property (eval, evalNF)
import           Hedgehog.Internal.Property (evalEither, evalExceptT, evalMaybe)
import           Hedgehog.Internal.Property (footnote, footnoteShow)
import           Hedgehog.Internal.Property (forAll, forAllWith)
import           Hedgehog.Internal.Property (LabelName)
import           Hedgehog.Internal.Property (Property)
import           Hedgehog.Internal.Property (Group(..), GroupName)
import           Hedgehog.Internal.Property (Confidence, verifiedTermination, withConfidence)
import           Hedgehog.Internal.Property (ShrinkLimit, withShrinks)
import           Hedgehog.Internal.Property (Skip, withSkip)
import           Hedgehog.Internal.Property (Test, property)
import           Hedgehog.Internal.Property (TestLimit, withTests)
import           Hedgehog.Internal.Property (collect, label)
import           Hedgehog.Internal.Range (Range, Size(..))
import           Hedgehog.Internal.Runner (check, recheck, recheckAt, checkSequential, checkParallel)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.Tripping (tripping)

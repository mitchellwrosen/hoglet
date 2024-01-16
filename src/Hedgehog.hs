module Hedgehog (
  -- * Properties
    Property
  , Group(..)

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

  , withDiscards

  , withShrinks

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
import           Hedgehog.Internal.Property (withDiscards)
import           Hedgehog.Internal.Property (eval, evalNF)
import           Hedgehog.Internal.Property (evalEither, evalExceptT, evalMaybe)
import           Hedgehog.Internal.Property (footnote, footnoteShow)
import           Hedgehog.Internal.Property (forAll, forAllWith)
import           Hedgehog.Internal.Property (LabelName)
import           Hedgehog.Internal.Property (Property)
import           Hedgehog.Internal.Property (Group(..))
import           Hedgehog.Internal.Property (Confidence, verifiedTermination, withConfidence)
import           Hedgehog.Internal.Property (withShrinks)
import           Hedgehog.Internal.Property (Skip, withSkip)
import           Hedgehog.Internal.Property (Test, property)
import           Hedgehog.Internal.Property (withTests)
import           Hedgehog.Internal.Property (collect, label)
import           Hedgehog.Internal.Range (Range, Size(..))
import           Hedgehog.Internal.Runner (check, recheck, recheckAt, checkSequential, checkParallel)
import           Hedgehog.Internal.Seed (Seed(..))
import           Hedgehog.Internal.Tripping (tripping)

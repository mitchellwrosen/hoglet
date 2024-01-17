module Hedgehog
  ( -- * Properties
    Property,
    Group (..),
    property,
    forAll,
    forAllWith,
    discard,
    check,
    recheck,
    recheckAt,
    checkGroup,
    withTests,
    withDiscards,
    withShrinks,
    withSkip,
    Skip,

    -- * Generating Test Data
    Gen,
    Range,
    Size (..),
    Seed (..),

    -- * Tests
    Test,
    annotate,
    annotateShow,
    footnote,
    footnoteShow,
    diff,
    (===),
    (/==),
    tripping,
    eval,
    evalNF,
    evalEither,
    evalMaybe,

    -- * Coverage
    LabelName,
    classify,
    cover,
    label,
    collect,
  )
where

import Hedgehog.Internal.Gen (Gen)
import Hedgehog.Internal.Property
  ( Group (..),
    LabelName,
    Property,
    Skip,
    Test,
    annotate,
    annotateShow,
    classify,
    collect,
    cover,
    diff,
    discard,
    eval,
    evalEither,
    evalMaybe,
    evalNF,
    footnote,
    footnoteShow,
    forAll,
    forAllWith,
    label,
    property,
    withDiscards,
    withShrinks,
    withSkip,
    withTests,
    (/==),
    (===),
  )
import Hedgehog.Internal.Range (Range, Size (..))
import Hedgehog.Internal.Runner (check, checkGroup, recheck, recheckAt)
import Hedgehog.Internal.Seed (Seed (..))
import Hedgehog.Internal.Tripping (tripping)

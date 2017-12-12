module Data.Sequence.Functional.Zippy where

import Prelude

import Data.Foldable (class Foldable)
import Data.Monoid (class Monoid)
import Data.Newtype (class Newtype)
import Data.Sequence.Functional (Sequence, repeat, zipWith)
import Data.Traversable (class Traversable)

newtype Zippy a = Zippy (Sequence a)

derive instance newtypeZippy :: Newtype (Zippy a) _

derive newtype instance eqZippy :: Eq a => Eq (Zippy a)

derive newtype instance ordZippy :: Ord a => Ord (Zippy a)

derive newtype instance semigroupZippy :: Semigroup (Zippy a)

derive newtype instance monoidZippy :: Monoid (Zippy a)

derive newtype instance foldableZippy :: Foldable Zippy

derive newtype instance traversableZippy :: Traversable Zippy

derive newtype instance functorZippy :: Functor Zippy

instance applyZippy :: Apply Zippy where
  apply (Zippy f) (Zippy a) = Zippy (zipWith ($) f a)

instance applicativeZippy :: Applicative Zippy where
  pure = Zippy <<< repeat
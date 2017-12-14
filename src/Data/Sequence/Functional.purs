module Data.Sequence.Functional where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Extend (class Extend)
import Control.Lazy (class Lazy)
import Control.MonadPlus (class MonadPlus)
import Control.MonadZero (class MonadZero)
import Control.Plus (class Plus)
import Data.Array as Array
import Data.Eq (class Eq1)
import Data.Foldable (class Foldable, foldlDefault, foldr, foldrDefault)
import Data.FoldableWithIndex (class FoldableWithIndex, foldlWithIndexDefault, foldrWithIndexDefault)
import Data.FunctorWithIndex (class FunctorWithIndex)
import Data.List as List
import Data.List.Lazy as Lazy
import Data.Maybe (Maybe(..), isNothing)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype, un)
import Data.Ord (class Ord1)
import Data.Traversable (class Traversable, sequenceDefault)
import Data.TraversableWithIndex (class TraversableWithIndex, traverseWithIndexDefault)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Unfoldable (class Unfoldable, unfoldr)
import Partial.Unsafe (unsafeCrashWith)

newtype Sequence a = Sequence (Int -> Maybe a)

derive instance newtypeSequence :: Newtype (Sequence a) _

index :: forall a. Sequence a -> Int -> Maybe a
index = un Sequence

infixl 8 index as !!

instance eqSequence :: Eq a => Eq (Sequence a) where
  eq = eqBy eq

instance eq1Sequence :: Eq1 Sequence where
  eq1 = eq

instance ordSequence :: Ord a => Ord (Sequence a) where
  compare = compareBy compare

instance ord1Sequence :: Ord1 Sequence where
  compare1 = compare

instance showSequence :: Show a => Show (Sequence a) where
  show x = "(fromFoldable " <> show (toUnfoldable x :: Array a) <> ")"

instance functorSequence :: Functor Sequence where
  map f x = Sequence \ n -> f <$> x !! n

-- TODO: Find a better implementation
instance applySequence :: Apply Sequence where
  apply f x = case uncons f of
    Just r -> (r.head <$> x) <> (r.tail <*> x)
    _ -> nil

instance applicativeSequence :: Applicative Sequence where
  pure a = Sequence \ i -> if i == 0 then Just a else Nothing

instance bindSequence :: Bind Sequence where
  bind x k = concat (map k x)

instance monadSequence :: Monad Sequence

instance semigroupSequence :: Semigroup (Sequence a) where
  append x y = Sequence z where
    l1 = length x
    l2 = length y
    z n
      | n < l1 = x !! n
      | otherwise = y !! (n - l1)

instance monoidSequence :: Monoid (Sequence a) where
  mempty = nil

instance unfoldableSequence :: Unfoldable Sequence where
  unfoldr f b = case f b of
    Just (Tuple a b1) -> a : unfoldr f b1
    _ -> nil

instance foldableSequence :: Foldable Sequence where
  foldMap :: forall a m. Monoid m => (a -> m) -> Sequence a -> m
  foldMap f x = go 0 mempty where
    go n acc
      | Just a <- x !! n = go (n + 1) (acc <> f a)
      | otherwise = acc
  foldr f = foldrDefault f
  foldl f = foldlDefault f

instance functorWithIndexSequence :: FunctorWithIndex Int Sequence where
  mapWithIndex f xs = Sequence \ i -> f i <$> xs !! i
  
instance foldableWithIndexSequence :: FoldableWithIndex Int Sequence where
  foldMapWithIndex :: forall a m. Monoid m => (Int -> a -> m) -> Sequence a -> m
  foldMapWithIndex f xs = go 0 mempty where
    go n acc
      | Just a <- xs !! n = go (n + 1) (acc <> f n a)
      | otherwise = acc
  foldrWithIndex f = foldrWithIndexDefault f
  foldlWithIndex f = foldlWithIndexDefault f

instance traversableSequence :: Traversable Sequence where
  traverse = go where
    go k xs = reverse <$> case uncons xs of
      Just r -> cons <$> k r.head <*> go k r.tail
      _ -> pure nil
  sequence xs = sequenceDefault xs

instance traversableWithIndex :: TraversableWithIndex Int Sequence where
  traverseWithIndex = traverseWithIndexDefault

instance lazySequence :: Lazy (Sequence a) where
  defer f = Sequence \ i -> f unit !! i

instance altSequence :: Alt Sequence where
  alt = append

instance plusSequence :: Plus Sequence where
  empty = nil

instance alternativeSequence :: Alternative Sequence

instance monadZeroSequence :: MonadZero Sequence

instance monadPlusSequence :: MonadPlus Sequence

instance extendSequence :: Extend Sequence where
  extend f = map f <<< tails

eqBy :: forall a. (a -> a -> Boolean) -> Sequence a -> Sequence a -> Boolean
eqBy p x y = go 0 where
  go n
    | Just a <- x !! n
    , Just b <- y !! n
    , p a b = go (n + 1)
    | Just a <- x !! n
    , Just b <- y !! n = p a b
    | Just _ <- x !! n
    , Nothing <- y !! n = false
    | Nothing <- x !! n
    , Just _ <- y !! n = false
    | otherwise = true

compareBy :: forall a. (a -> a -> Ordering) -> Sequence a -> Sequence a -> Ordering
compareBy p x y = go 0 where
  go n
    | Just a <- x !! n
    , Just b <- y !! n
    , p a b == EQ = go (n + 1)
    | Just _ <- x !! n
    , Nothing <- y !! n = GT
    | Nothing <- x !! n
    , Just _ <- y !! n = LT
    | Just a <- x !! n
    , Just b <- y !! n = p a b
    | otherwise = EQ

fromFoldable :: forall f a. Foldable f => f a -> Sequence a
fromFoldable = foldr cons nil

fromIndexable :: forall f a. (f a -> Int -> Maybe a) -> f a -> Sequence a
fromIndexable getIndex = Sequence <<< getIndex

fromArray :: Array ~> Sequence
fromArray = fromIndexable Array.index

fromList :: List.List ~> Sequence
fromList = fromIndexable List.index

fromLazyList :: Lazy.List ~> Sequence
fromLazyList = fromIndexable Lazy.index

toUnfoldable :: forall f. Unfoldable f => Sequence ~> f
toUnfoldable xs = unfoldr f 0 where
  f n = case xs !! n of
    Just a -> Just (Tuple a (n + 1))
    _ -> Nothing

nil :: forall a. Sequence a
nil = Sequence (const Nothing)

cons :: forall a. a -> Sequence a -> Sequence a
cons a xs = Sequence y where
  y 0 = Just a
  y n = xs !! (n - 1)

infixr 5 cons as :

length :: forall a. Sequence a -> Int
length x = go 0 where
  go n
    | Just _ <- x !! n = go (n + 1)
    | otherwise = n

null :: forall a. Sequence a -> Boolean
null x = isNothing (x !! 0)

concat :: forall f a. Foldable f => f (Sequence a) -> Sequence a
concat = foldr append nil

head :: Sequence ~> Maybe
head = index <@> 0

tail :: forall a. Sequence a -> Maybe (Sequence a)
tail xs = case head xs of
  Nothing -> Nothing
  _ -> Just (Sequence \ n -> xs !! (n + 1))

tails :: forall a. Sequence a -> Sequence (Sequence a)
tails xs = Sequence f where
  l = length xs
  f i
    | i <= l = Just (drop i xs)
    | otherwise = Nothing

uncons :: forall a. Sequence a -> Maybe {head :: a, tail :: Sequence a}
uncons xs = {head: _, tail: _} <$> head xs <*> tail xs

init :: forall a. Sequence a -> Maybe (Sequence a)
init xs = if null xs
  then Nothing
  else Just (Sequence \ n -> if n == length xs - 1 then Nothing else xs !! n)

last :: Sequence ~> Maybe
last xs = xs !! (length xs - 1)

take :: Int -> Sequence ~> Sequence
take n xs = Sequence f where
  f i
    | i < n = xs !! i
    | otherwise = Nothing

drop :: Int -> Sequence ~> Sequence
drop n xs = Sequence \ i -> xs !! (n + i)

snoc :: forall a. Sequence a -> a -> Sequence a
snoc xs x = Sequence \ i -> if i == length xs then Just x else xs !! i

unsnoc :: forall a. Sequence a -> Maybe { init :: Sequence a, last :: a }
unsnoc xs = {init: _, last: _} <$> init xs <*> last xs

concatMap :: forall a b. (a -> Sequence b) -> Sequence a -> Sequence b
concatMap = flip bind

zipWith :: forall a b c. (a -> b -> c) -> Sequence a -> Sequence b -> Sequence c
zipWith f xs ys = Sequence \ i -> f <$> xs !! i <*> ys !! i

zip :: forall a b. Sequence a -> Sequence b -> Sequence (Tuple a b)
zip = zipWith Tuple

zap :: forall a b. Sequence (a -> b) -> Sequence a -> Sequence b
zap = zipWith ($)

unzip :: forall a b. Sequence (Tuple a b) -> Tuple (Sequence a) (Sequence b)
unzip xs = Tuple (map fst xs) (map snd xs)

repeat :: forall a. a -> Sequence a
repeat x = Sequence \ _ -> Just x

iterate :: forall a. (a -> a) -> a -> Sequence a
iterate f seed = Sequence g where
  g 0 = Just seed
  g n = f <$> g (n - 1)

cycle :: Sequence ~> Sequence
cycle xs = Sequence \ i -> xs !! (i `mod` length xs)

reverse :: Sequence ~> Sequence
reverse xs = Sequence \ i -> xs !! (length xs - i - 1)

newtype Step = Step Int

-- | [n,i..m]
rangeBy :: Step -> Int -> Int -> Sequence Int
rangeBy (Step n) start end = Sequence f where
  f i
    | i <= end/n - start = Just (i*n + start)
    | otherwise = Nothing

-- | [n..m]
range :: Int -> Int -> Sequence Int
range = rangeBy (Step 1)

infix 8 range as ..

-- | [n,i..]
rayBy :: Step -> Int -> Sequence Int
rayBy (Step n) start = Sequence \ i -> Just (i*n + start)

-- | [n..]
ray :: Int -> Sequence Int
ray = rayBy (Step 1)

findIndexWithIndex :: forall a. (Int -> a -> Boolean) -> Sequence a -> Maybe Int
findIndexWithIndex f xs = go 0 where
  go n = case xs !! n of
    Just a -> if f n a then Just n else go (n + 1)
    _ -> Nothing

findIndex :: forall a. (a -> Boolean) -> Sequence a -> Maybe Int
findIndex = findIndexWithIndex <<< const

findLastIndex :: forall a. (a -> Boolean) -> Sequence a -> Maybe Int
findLastIndex f xs = go (length xs - 1) where
  go n = case xs !! n of
    Just a -> if f a then Just n else go (n - 1)
    _ -> Nothing

filter :: forall a. (a -> Boolean) -> Sequence a -> Sequence a
filter f xs = Sequence (go (-1)) where
  go prevIdx = g (prevIdx + 1) 
  g acc 0
    | Just a <- xs !! acc
    , f a = Just a
    | Just _ <- xs !! acc = g (acc + 1) 0
    | otherwise = Nothing
  g acc n
    | Just a <- xs !! acc
    , f a = g (acc + 1) (n - 1)
    | Just _ <- xs !! acc = g (acc + 1) n
    | otherwise = Nothing

unsafeIndex :: forall a. Partial => Sequence a -> Int -> a
unsafeIndex xs i = case xs !! i of
  Just a -> a
  _ -> unsafeCrashWith $
    "unsafeIndex called out of bounds on index " <>
    show i <>
    " but sequence has length " <>
    show (length xs)
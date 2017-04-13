module Data.Vec
  ( Vec
  , empty
  , cons, (+>)
  , snoc
  , uncons
  , singleton
  , replicate
  , length
  , lengthT
  , toArray
  , toUnfoldable
  , index, (!!)
  , concat
  , updateAt
  , modifyAt
  , insertAt
  , deleteAt
  , head
  , last
  , tail
  , init
  , insert
  , insertBy
  , slice
  , take
  , drop
  , zip
  , zipWith
  , unzip
  , sort
  , sortBy
  , reverse
  ) where

import Prelude
import Data.Array as Array
import Data.Foldable (foldl, foldr, foldMap, class Foldable)
import Data.Maybe (fromJust)
import Data.Traversable (traverse, sequence, class Traversable)
import Data.Tuple (Tuple(Tuple))
import Data.Typelevel.Num (class Min, class Sub, class LtEq, class Pred, class Lt)
import Data.Typelevel.Num.Ops (class Add, class Succ)
import Data.Typelevel.Num.Reps (D1, D0)
import Data.Typelevel.Num.Sets (toInt, class Pos, class Nat)
import Data.Typelevel.Undefined (undefined)
import Data.Unfoldable (class Unfoldable)
import Partial.Unsafe (unsafePartial)

-- | `Vec s a` is an array with a fixed size `s` defined at the type level.
newtype Vec s a = Vec (Array a)

-- | An empty vector.
empty :: forall a. Vec D0 a
empty = Vec []

-- | Prepend a value to the front of a vector, creating a vector of size `Succ s`.
cons :: forall s s' a. Succ s s' => a -> Vec s a -> Vec s' a
cons x (Vec xs) = Vec $ Array.cons x xs
infixr 5 cons as +>

-- | Append a value to the end of a vector, creating a vector of size `Succ s`.
snoc :: forall s s' a. Succ s s' => a -> Vec s a -> Vec s' a
snoc x (Vec xs) = Vec $ Array.snoc xs x

-- | Get the head and the tail of a non-empty vector.
uncons :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> { head :: a, tail :: Vec s2 a }
uncons (Vec v) = case unsafePartial $ fromJust $ Array.uncons v of
  { head: h, tail: t } -> { head: h, tail: Vec t }

-- | Construct a vector containing only a single element.
singleton :: forall a. a -> Vec D1 a
singleton x = x +> empty

-- | Construct a vector of a given length containing the same element repeated.
replicate :: forall s a. Nat s => s -> a -> Vec s a
replicate s a = Vec $ Array.replicate (toInt s) a

-- | Get the length of a vector as an integer.
length :: forall s a. Nat s => Vec s a -> Int
length _ = toInt (undefined :: s)

-- | Get the length of a vector as a type level number.
lengthT :: forall s a. Nat s => Vec s a -> s
lengthT _ = undefined

-- | Convert a vector into an array. This simply unwraps the underlying array, so it has no runtime cost.
toArray :: forall s a. Nat s => Vec s a -> Array a
toArray (Vec xs) = xs

-- | Convert a vector into any `Unfoldable`.
toUnfoldable :: forall f s a. Unfoldable f => Nat s => Vec s a -> f a
toUnfoldable (Vec v) = Array.toUnfoldable v

-- | Get the element at a given index inside a vector. Index out of bounds errors
-- | are caught at compile time.
-- |
-- | Example:
-- |
-- |     myVector = 1 +> 2 +> 3 +> 4 +> empty
-- |     value = index myVector d2
-- |     -- value == 3
-- |     value = index myVector d4
-- |     -- out of bounds so does not type check
index :: forall i s a. Nat i => Lt i s => Vec s a -> i -> a
index (Vec xs) i = unsafePartial $ Array.unsafeIndex xs $ toInt i
infixl 8 index as !!

-- | Concatenate two vectors together.
concat :: forall s1 s2 s3 a. Add s1 s2 s3 => Vec s1 a -> Vec s2 a -> Vec s3 a
concat (Vec xs1) (Vec xs2) = Vec $ Array.concat [xs1, xs2]

-- | Update a vector with a given value inserted at a given index.
updateAt :: forall i s a. Nat i => Lt i s => i -> a -> Vec s a -> Vec s a
updateAt i v (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.updateAt (toInt i) v xs

-- | Update a vector at a given index using a function.
modifyAt :: forall i s a. Nat i => Lt i s => i -> (a -> a) -> Vec s a -> Vec s a
modifyAt i f (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.modifyAt (toInt i) f xs

-- | Insert a value at a given index inside a vector, returning a vector
-- | that is one element larger.
insertAt :: forall i s1 s2 a. Nat i => Lt i s1 => Succ s1 s2 => i -> a -> Vec s1 a -> Vec s2 a
insertAt i a (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.insertAt (toInt i) a xs

-- | Remove an element at a given index inside a vector, returning a vector
-- | that is one element smaller.
deleteAt :: forall i s1 s2 a. Nat i => Lt i s1 => Pred s1 s2 => i -> Vec s1 a -> Vec s2 a
deleteAt i (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.deleteAt (toInt i) xs

-- | Get the head of a non-empty vector.
head :: forall s a. Pos s => Vec s a -> a
head (Vec xs) = unsafePartial $ fromJust $ Array.head xs

-- | Get the last element of a non-empty vector.
last :: forall s a. Pos s => Vec s a -> a
last (Vec xs) = unsafePartial $ fromJust $ Array.last xs

-- | Get the tail of a non-empty vector.
tail :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> Vec s2 a
tail (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.tail xs

-- | Get all but the last element of a non-empty vector.
init :: forall s1 s2 a. Pred s1 s2 => Vec s1 a -> Vec s2 a
init (Vec xs) = Vec $ unsafePartial $ fromJust $ Array.init xs

-- | Insert an element into a sorted vector.
insert :: forall s1 s2 a. Succ s1 s2 => Ord a => a -> Vec s1 a -> Vec s2 a
insert a (Vec v) = Vec $ Array.insert a v

-- | Insert an element into a sorted vector using an ordering function.
insertBy :: forall s1 s2 a. Succ s1 s2 => (a -> a -> Ordering) -> a -> Vec s1 a -> Vec s2 a
insertBy f a (Vec v) = Vec $ Array.insertBy f a v

-- | Get a sub-vector from index `i1` up to but not including index `i2`.
slice :: forall i1 i2 s1 s2 a. Nat i1 => Nat i2 => LtEq i1 s1 => LtEq i2 s1 => LtEq i1 i2 => Sub i2 i1 s2 => i1 -> i2 -> Vec s1 a -> Vec s2 a
slice i1 i2 (Vec xs) = Vec $ Array.slice (toInt i1) (toInt i2) xs

-- | Get the first `c` elements from a vector.
take :: forall c s a. Nat c => LtEq c s => c -> Vec s a -> Vec c a
take c (Vec xs) = Vec $ Array.take (toInt c) xs

-- | Drop the first `c` elements from a vector.
drop :: forall c s1 s2 a. Nat c => LtEq c s1 => Sub s1 c s2 => c -> Vec s1 a -> Vec s2 a
drop c (Vec xs) = Vec $ Array.drop (toInt c) xs

-- | Zip two vectors together into a vector of tuples.
-- |
-- | The new vector will be the size of the smallest input vector, and
-- | superfluous elements from the other will be discarded.
zip :: forall s1 s2 s3 a b. Min s1 s2 s3 => Vec s1 a -> Vec s2 b -> Vec s3 (Tuple a b)
zip (Vec v1) (Vec v2) = Vec $ Array.zip v1 v2

-- | Zip two vectors together using a combining function.
-- |
-- | The new vector will be the size of the smallest input vector, and
-- | superfluous elements from the other will be discarded.
zipWith :: forall s1 s2 s3 a b c. Nat s1 => Nat s2 => Min s1 s2 s3 => (a -> b -> c) -> Vec s1 a -> Vec s2 b -> Vec s3 c
zipWith f (Vec v1) (Vec v2) = Vec $ Array.zipWith f v1 v2

-- | Unzip a vector of tuples into a tuple of vectors.
unzip :: forall s a b. Nat s => Vec s (Tuple a b) -> Tuple (Vec s a) (Vec s b)
unzip (Vec v) = case Array.unzip v of
  (Tuple v1 v2) -> Tuple (Vec v1) (Vec v2)

-- | Sort a vector of `Ord`s.
sort :: forall s a. Nat s => Ord a => Vec s a -> Vec s a
sort (Vec v) = Vec $ Array.sort v

-- | Sort a vector using an ordering function.
sortBy :: forall s a. Nat s => (a -> a -> Ordering) -> Vec s a -> Vec s a
sortBy f (Vec v) = Vec $ Array.sortBy f v

-- | Reverse a vector.
reverse :: forall s a. Nat s => Vec s a -> Vec s a
reverse (Vec v) = Vec $ Array.reverse v

instance functorVec :: Nat s => Functor (Vec s) where
  map f (Vec xs) = Vec $ map f xs

instance applyVec :: Nat s => Apply (Vec s) where
  apply (Vec a) (Vec b) = Vec $ apply a b

instance applicativeVec :: Nat s => Applicative (Vec s) where
  pure a = replicate (undefined :: s) a

instance foldableVec :: Nat s => Foldable (Vec s) where
  foldMap f (Vec xs) = foldMap f xs
  foldr f i (Vec xs) = foldr f i xs
  foldl f i (Vec xs) = foldl f i xs

instance traversableVec :: Nat s => Traversable (Vec s) where
  traverse f (Vec xs) = Vec <$> traverse f xs
  sequence (Vec xs) = Vec <$> sequence xs

instance eqVec :: (Nat s, Eq a) => Eq (Vec s a) where
  eq (Vec v1) (Vec v2) = v1 == v2

instance showVec :: (Nat s, Show a) => Show (Vec s a) where
  show (Vec v) = show v

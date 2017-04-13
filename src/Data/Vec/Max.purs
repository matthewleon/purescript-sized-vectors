module Data.Vec.Max where

import Prelude
import Data.Typelevel.Num (class GtEq)
import Data.Typelevel.Num.Ops (class Succ)
import Data.Typelevel.Num.Reps (D1, D0)
import Data.Typelevel.Num.Sets (class Nat, class Pos)
import Data.Vec as V

newtype Vec m s a = MaxVec (V.Vec s a)

-- | An empty vector
empty :: forall m a. Nat m => Vec m D0 a
empty = MaxVec V.empty

-- | Prepend a value to the front of a vector.
cons ::
   forall m s s' a.
   Succ s s' => GtEq m s'
=> a -> Vec m s a -> Vec m s' a
cons x (MaxVec xs) = MaxVec $ V.cons x xs
infixr 5 cons as +>

-- | Construct a vector containing only a single element.
singleton :: forall m a. GtEq m D1 => a -> Vec m D1 a
singleton x = x +> empty

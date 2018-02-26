module Data.Vec.Gen (genVec) where

import Prelude

import Control.Monad.Gen (class MonadGen, resize, unfoldable)
import Control.Monad.Rec.Class (class MonadRec)
import Data.Maybe (fromJust)
import Data.Vec (Vec, fromArray)
import Partial.Unsafe (unsafePartial)
import Type.Data.Nat (class Nat, NProxy(..), toInt)

genVec :: forall m s a. MonadRec m => MonadGen m => Nat s => m a -> m (Vec s a)
genVec gen = unsafePartial fromJust <<< fromArray <$>
  resize (const $ toInt (NProxy :: NProxy s)) (unfoldable gen)

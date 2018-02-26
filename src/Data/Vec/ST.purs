module Data.Vec.ST where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Array.ST (STArray)
import Data.Array.ST as STA
import Data.Array.ST.Partial as STAP
import Data.ModularArithmetic (Z, runZ)
import Data.Vec as V
import Partial.Unsafe (unsafePartial)
import Type.Data.Nat (class Lt, class Nat, D0, NProxy, toInt, kind Nat)
import Unsafe.Coerce (unsafeCoerce)

newtype Vec h (s :: Nat) a = Vec (STArray h a)

unsafeFreeze :: forall a s r h. Vec h s a -> Eff (st :: ST h | r) (V.Vec s a)
unsafeFreeze = pure <<< (unsafeCoerce :: Vec h s a -> V.Vec s a)

empty :: forall a r h. Eff (st :: ST h | r) (Vec h D0 a)
empty = Vec <$> STA.emptySTArray

thaw :: forall a s r h. V.Vec s a -> Eff (st :: ST h | r) (Vec h s a)
thaw = map Vec <<< STA.thaw <<< V.toArray

freeze :: forall a s r h. Vec h s a -> Eff (st :: ST h | r) (V.Vec s a)
freeze (Vec sta) = unsafeCoerce STA.freeze sta

peek
  :: forall i s a r h
   . Nat i
  => Lt i s
  => Vec h s a -> NProxy i -> Eff (st :: ST h | r) a
peek (Vec sta) = unsafePartial STAP.peekSTArray sta <<< toInt

peekZ :: forall s a r h. Vec h s a -> Z s -> Eff (st :: ST h | r) a
peekZ (Vec sta) = unsafePartial STAP.peekSTArray sta <<< runZ

poke
  :: forall i s a r h
   . Nat i
  => Lt i s
  => Vec h s a -> NProxy i -> a -> Eff (st :: ST h | r) Unit
poke (Vec sta) n = unsafePartial STAP.pokeSTArray sta (toInt n)

pokeZ :: forall s a r h. Vec h s a -> Z s -> a -> Eff (st :: ST h | r) Unit
pokeZ (Vec sta) z = unsafePartial STAP.pokeSTArray sta $ runZ z

module Test.Data.Vec.ST where

import Prelude

import Control.Apply (lift2, lift3)
import Control.Monad.Eff.Random (RANDOM)
import Control.Monad.ST (pureST)
import Data.ModularArithmetic (genZ)
import Data.Vec (Vec, indexZ, modifyAtZ, updateAtZ)
import Data.Vec.Gen (genVec)
import Data.Vec.ST as ST
import Test.QuickCheck (arbitrary)
import Test.QuickCheck.Gen (Gen)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.QuickCheck (quickCheck)
import Type.Data.Nat (D9)

stVecTests ∷ ∀ e. TestSuite (random ∷ RANDOM | e)
stVecTests = suite "stVec" do
  let arbVec = genVec arbitrary :: Gen (Vec D9 Int)
  test "thaw/freeze" $ quickCheck $
    (\v -> v == pureST (ST.freeze =<< ST.thaw v)) <$> arbVec
  test "thaw/unsafeFreeze" $ quickCheck $
    (\v -> v == pureST (ST.unsafeFreeze =<< ST.thaw v)) <$> arbVec
  test "peek" $ quickCheck $
    lift2 (\v z -> indexZ v z == pureST (flip ST.peekZ z =<< ST.thaw v))
          arbVec
          genZ
  test "poke" $ quickCheck $
    lift3 (\v z x -> updateAtZ z x v == pureST do
             stv <- ST.thaw v
             ST.pokeZ stv z x
             ST.unsafeFreeze stv
          ) arbVec genZ arbitrary
  test "modify" $ quickCheck $
    lift3 (\v z f -> modifyAtZ z f v == pureST do
             stv <- ST.thaw v
             ST.modifyZ stv z f
             ST.unsafeFreeze stv
          ) arbVec genZ (add <$> arbitrary)

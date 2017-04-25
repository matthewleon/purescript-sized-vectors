module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Data.Traversable (sequence)
import Data.Typelevel.Num (toInt, d3, D1, D2, D3, D9)
import Data.Vec (Vec, slice, tail, drop, take, lengthT, concat, replicate, (+>), empty)
import Prelude (($), Unit, pure, discard)
import Test.Unit (suite, test)
import Test.Unit.Assert (equal)
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall e. Eff (console :: CONSOLE, testOutput :: TESTOUTPUT, avar :: AVAR | e) Unit
main = runTest do
  suite "vec" do
    let (vec1 :: Vec D2 Int) = replicate 1
        (vec2 :: Vec D3 Int) = replicate 2
        (vec3 :: Vec D9 Int) = replicate 3
    test "cons length" do
      equal 3 $ toInt $ lengthT $ 1 +> 2 +> 3 +> empty
    test "replicate length" do
      equal 2 $ toInt $ lengthT vec1
    test "concat length" do
      equal 5 $ toInt $ lengthT (concat vec1 vec2)
    test "take length" do
      equal 2 $ toInt $ lengthT (take vec2 :: Vec D2 Int)
    test "drop length" do
      equal 1 $ toInt $ lengthT (drop vec2 :: Vec D1 Int)
    test "tail length" do
      equal 1 $ toInt $ lengthT (tail vec1)
    test "slice length" do
      equal 3 $ toInt $ lengthT (slice d3 vec3 :: Vec D3 Int)
    test "pure replicates" do
      let vec3' = pure 3 :: Vec D9 Int
      equal vec3 vec3'
    test "traversable 1" do
      let vecOfArrays = [1] +> [2] +> [3] +> empty
          expected = 1 +> 2 +> 3 +> empty
      equal [expected] $ sequence vecOfArrays
    test "traversable 2" do
      let vecOfArrays = [1,2] +> [2,3] +> empty
          expected = [ 1 +> 2 +> empty
                     , 1 +> 3 +> empty
                     , 2 +> 2 +> empty
                     , 2 +> 3 +> empty
                     ]
      equal expected $ sequence vecOfArrays

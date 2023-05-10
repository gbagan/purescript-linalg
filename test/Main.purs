module Test.Main where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Rational ((%))
import Effect (Effect)
import Effect.Aff (launchAff_)
import LinearAlgebra.Matrix as M
import LinearAlgebra.Vector as V
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner (runSpec)

main :: Effect Unit
main = launchAff_ $ runSpec [consoleReporter] do
  describe "linalg" do
    describe "vectors" do
      let v1 = V.fromArray [2, 3, 1, 0]
      let v2 = V.fromArray [3, 4, 0, 1]
      let v3 = V.fromArray [1%1, 2%1, 3%1]
      let v4 = V.fromArray [2%1, 4%1, 6%1]
      let v5 = V.fromArray [2%1, 4%1, 7%1]

      describe "operations" do
        it "sum" do
          (v1 `V.add` v2) `shouldEqual` V.fromArray [5, 7, 1, 1]
        it "difference" do
          (v1 `V.diff` v2) `shouldEqual` V.fromArray [-1, -1, 1, -1]
        it "scalar multiplication" do
          (2 `V.scale` v1) `shouldEqual` V.fromArray [4, 6, 2, 0]
        it "dot product" do
          (v1 `V.dot` v2) `shouldEqual` 18
        it "colinear" do
          (v3 `V.colinear` v4) `shouldEqual` true
          (v3 `V.colinear` v5) `shouldEqual` false

    describe "matrices" do
      let m1 = M.fromArray 2 4 [ [2, 3, 1, 0]
                               , [3, 4, 0, 1]
                               ]
      let m2 = M.fromArray 2 4 [[4, 3, 2, 3], [2, 2, 1, -8]]
      let m3 = M.fromArray 4 2 [[-1, 3], [2, 3], [3, 1], [4, -3]]
      let m4 = M.fromArray 2 2 [[-1%1, 3%2], [1%1, -1%1]]
      let m5 = M.fromArray 3 3 [ [2%1, -1%1, 0%1]
                               , [0%1, -1%1, 2%1]
                               , [-1%1, 2%1, -1%1]
                               ]
      let m6 = M.fromArray 2 2 [[0%1, 1%1], [1%1, 0%1]]
      let m7 = M.fromArray 3 2 [[1%1,2%1], [2%1, 4%1], [3%1, 6%1]]
      let m8 = M.fromArray 3 3 [[1%1, 1%1, 0%1], [0%1, 1%1, 1%1], [1%1, 2%1, 1%1]]

      describe "constructors" do
        it "fromColumns" do
          M.fromColumns 3 3 (M.columns m5) `shouldEqual` m5

      describe "operations" do
        it "sum" do
          (m1 `M.add` m2) `shouldEqual` M.fromArray 2 4 [[6, 6, 3, 3], [5, 6, 1, -7]]
        it "difference" do
          (m1 `M.diff` m2) `shouldEqual` M.fromArray 2 4 [[-2, 0, -1, -3], [1, 2, -1, 9]]
        it "product" do
          (m1 `M.mult` m3) `shouldEqual` M.fromArray 2 2 [[7,16],[9,18]]
        it "transpose" do
          M.transpose m1 `shouldEqual` M.fromArray 4 2 [[2,3],[3,4],[1,0],[0,1]]
        it "inverse 2x2" do
          let m4' = M.fromArray 2 2 [[2%1,3%1],[2%1,2%1]]
          M.inverse m4 `shouldEqual` Just m4'
        it "inverse 3x3" do
          let m5' = M.fromArray 3 3 [[3%4,1%4,1%2],[1%2,1%2,1%1],[1%4,3%4,1%2]]
          M.inverse m5 `shouldEqual` Just m5'
        it "inverse 2x2 bis" do
          M.inverse m6 `shouldEqual` Just m6
        it "inverse (not inversible)" do
          M.inverse m8 `shouldEqual` Nothing
        it "determinant 1" do
          M.determinant m4 `shouldEqual` (-1 % 2)
        it "determinant 2" do
          M.determinant m5 `shouldEqual` (-4 % 1)
        it "determinant 3" do
          M.determinant m6 `shouldEqual` (-1 % 1)
        it "determinant 4" do
          M.determinant m8 `shouldEqual` (0%1)
        it "image" do
          Array.length (M.image m5) `shouldEqual` 3
        it "kernel 1" do
          M.kernel m5 `shouldEqual` []
        it "kernel 2" do
          M.kernel m7 `shouldSatisfy` \m -> Array.length m == 1 
                                         && Array.all (_ `V.colinear` V.fromArray [2%1, -1%1]) m
      describe "solve linear system" do
        it "system 1" do
          let b = V.fromArray [1%1, 3%1, 2%1]
          let x = V.fromArray [5%2, 4%1, 7%2]
          M.solveLinearSystem m5 b `shouldEqual` (Just {sol: x, basis: []})
        it "system 2" do
          let m = M.fromArray 3 4 [ [1%1, 2%1, 0%1, 0%1]
                                  , [0%1, 3%1, 4%1, 0%1]
                                  , [0%1, 0%1, 5%1, 6%1]
                                  ]
          let b = V.fromArray [7%1, 8%1, 9%1]
          case M.solveLinearSystem m b of
            Just {sol, basis} -> do
              sol `shouldSatisfy` \v -> M.mult' m v == b 
              basis `shouldSatisfy` \bs -> Array.length bs == 1 
                                        && Array.all (_ `V.colinear` V.fromArray [-16%5, 8%5, -6%5, 1%1]) bs
            Nothing -> pure unit
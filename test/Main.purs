module Test.Main where
import Prelude
import Data.Array as Array
import Data.Rational ((%))
import LinearAlgebra.Vector as V
import LinearAlgebra.Matrix as M
import Effect (Effect)
import Effect.Aff (launchAff_)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy, shouldNotSatisfy)
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
          (v1 `V.sum` v2) `shouldEqual` V.fromArray [5, 7, 1, 1]
        it "difference" do
          (v1 `V.diff` v2) `shouldEqual` V.fromArray [-1, -1, 1, -1]
        it "dot product" do
          (v1 `V.dot` v2) `shouldEqual` 18
        it "colinear" do
          (v3 `V.colinear` v4) `shouldEqual` true
          (v3 `V.colinear` v5) `shouldEqual` false

    describe "matrices" do
      let m1 = M.fromArray [[2, 3, 1, 0], [3, 4, 0, 1]]
      let m2 = M.fromArray [[4, 3, 2, 3], [2, 2, 1, -8]]
      let m3 = M.fromArray [[-1, 3], [2, 3], [3, 1], [4, -3]]
      let m4 = M.fromArray [[-1%1, 3%2], [1%1, -1%1]]
      let m5 = M.fromArray [[2%1, -1%1, 0%1], [0%1, -1%1, 2%1], [-1%1, 2%1, -1%1]]
      let m6 = M.fromArray [[0%1, 1%1], [1%1, 0%1]]
      let m7 = M.fromArray [[1%1,2%1], [2%1, 4%1], [3%1, 6%1]]

      describe "invalid matrices" do
        it "empty matrix" do
          (M.fromArray [] :: M.Matrix Int) `shouldNotSatisfy` M.isValid
        it "matrix with empty columns" do
          M.isValid (M.fromArray [[], []] :: M.Matrix Int) `shouldEqual` false
        it "matrix with different column sizes" do
          M.isValid (M.fromArray [[3, 4], [1]]) `shouldEqual` false  
      describe "operations" do
        it "sum" do
          (m1 `M.sum` m2) `shouldEqual` M.fromArray [[6, 6, 3, 3], [5, 6, 1, -7]]
        it "difference" do
          (m1 `M.diff` m2) `shouldEqual` M.fromArray [[-2, 0, -1, -3], [1, 2, -1, 9]]
        it "product" do
          (m1 `M.product` m3) `shouldEqual` M.fromArray [[7,16],[9,18]]
        it "transpose" do
          M.transpose m1 `shouldEqual` M.fromArray [[2,3],[3,4],[1,0],[0,1]]
        it "inverse" do
          M.inverse m4 `shouldEqual` M.fromArray [[2 % 1,3 % 1],[2 % 1,2 % 1]]
          M.inverse m5 `shouldEqual` M.fromArray [[3 % 4,1 % 4,1 % 2],[1 % 2,1 % 2,1 % 1],[1 % 4,3 % 4,1 % 2]]
          M.inverse m6 `shouldEqual` m6
        it "determinant" do
          M.determinant m4 `shouldEqual` (-1 % 2)
          M.determinant m5 `shouldEqual` (-4 % 1)
          M.determinant m6 `shouldEqual` (-1 % 1)
        it "image" do
          Array.length (M.image m5) `shouldEqual` 3
        it "kernel" do
          M.kernel m5 `shouldEqual` []
        it "kernel" do
          M.kernel m7 `shouldSatisfy` \m -> Array.length m == 1 
                                            && Array.all (\v -> v `V.colinear` V.fromArray [2%1, -1%1]) m
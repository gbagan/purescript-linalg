module Test.Main where
import Prelude
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import LinearAlgebra.Matrix as M

main :: Effect Unit
main = do
    let m = M.fromArray [[1.0, 2.0], [3.0, 4.0]]
    log $ "m = " <> show m
    log $ show $ m # M.mapWithIndex \i j x -> if i == j then x else 0.0
    log $ "squared m: " <> show (M.product m m)
    log $ "gaussJordan: " <> show (M.gaussJordan m)
    let m2 = M.fromArray [[2.0, 3.0, 1.0, 0.0], [3.0, 4.0, 0.0, 1.0]]
    log $ "m2 = " <> show m2
    --log $ "column 0 m2: " <> show (M.column 0 m2)
    --log $ "row 0 m2: " <> show (M.row 0 m2)
    --log $ "transpose m2: " <> show (M.transpose m2)
    --log $ "m2 * tr(m2): " <> show (m2 `M.product` M.transpose m2)
    log $ "gaussJordan m2: " <> show (M.gaussJordan m2)
    let m3 = M.fromArray [[2.0, -1.0, 0.0], [-1.0, 2.0, -1.0], [0.0, -1.0, 2.0]]
    log $ "m3 = " <> show m3
    --log $ "column 0 m2: " <> show (M.column 0 m2)
    --log $ "row 0 m2: " <> show (M.row 0 m2)
    --log $ "transpose m2: " <> show (M.transpose m2)
    --log $ "m2 * tr(m2): " <> show (m2 `M.product` M.transpose m2)
    log $ "m3 * m3': " <> show (m3 `M.product` M.inverse m3)
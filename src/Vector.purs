module LinearAlgebra.Vector
  ( Vector
  , fromArray
  , invalid
  , sum
  , diff
  , dot
  , mult
  , opposite
  )
  where

import Prelude
import Data.Array (foldl, length, zipWith)

data Vector a = Vector (Array a) | Invalid

fromArray :: forall a. Array a -> Vector a
fromArray = Vector

invalid :: forall a. Vector a
invalid = Invalid

sum :: forall a. Semiring a => Vector a -> Vector a -> Vector a
sum (Vector v1) (Vector v2)
    | length v1 == length v2 = Vector $ zipWith (+) v1 v2
sum _ _ = Invalid

diff :: forall a. Ring a => Vector a -> Vector a -> Vector a
diff (Vector v1) (Vector v2) 
    | length v1 == length v2 = Vector $ zipWith (-) v1 v2
diff _ _ = Invalid

dot :: forall a. Semiring a => Vector a -> Vector a -> a
dot (Vector v1) (Vector v2)
    | length v1 == length v2 = foldl (+) zero $ zipWith (*) v1 v2
dot _ _ = zero

mult :: forall a. Semiring a => a -> Vector a -> Vector a
mult a (Vector v) = Vector $ map (_ * a) v
mult _ _ = Invalid

opposite :: forall a. Ring a => Vector a -> Vector a
opposite (Vector v) = Vector $ map (zero - _) v
opposite _ = Invalid
module LinearAlgebra.Vector
  ( Vector
  , fromArray
  , invalid
  , sum
  , diff
  , dot
  , multBy
  , opposite
  )
  where

import Prelude
import Data.Array (foldl, length, zipWith)

data Vector a = Vector (Array a) | Invalid
derive instance Eq a => Eq (Vector a)
derive instance Functor Vector

instance Show a => Show (Vector a) where
    show Invalid = "Invalid vector"
    show (Vector v) = "(Vatrix " <> show v <> ")" 

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

multBy :: forall a. Semiring a => a -> Vector a -> Vector a
multBy a (Vector v) = Vector $ map (_ * a) v
multBy _ _ = Invalid

opposite :: forall a. Ring a => Vector a -> Vector a
opposite (Vector v) = Vector $ map (zero - _) v
opposite _ = Invalid
module LinearAlgebra.Vector
  ( Vector
  , fromArray
  , invalid
  , null
  , add
  , diff
  , dot
  , smult
  , opposite
  , colinear
  )
  where

import Prelude hiding (add)
import Data.Array (all, foldl, length, zipWith, uncons)
import Data.Maybe (Maybe(..))

data Vector a = Vector (Array a) | Invalid
derive instance Eq a => Eq (Vector a)
derive instance Functor Vector

instance Show a => Show (Vector a) where
    show Invalid = "Invalid vector"
    show (Vector v) = "(Vector " <> show v <> ")" 

fromArray :: forall a. Array a -> Vector a
fromArray = Vector

invalid :: forall a. Vector a
invalid = Invalid

-- | tests if the vector is null i.e. contains only zero values
null :: forall a. Eq a => Semiring a => Vector a -> Boolean
null (Vector v) = all (_ == zero) v
null Invalid = true

add :: forall a. Semiring a => Vector a -> Vector a -> Vector a
add (Vector v1) (Vector v2)
    | length v1 == length v2 = Vector $ zipWith (+) v1 v2
add _ _ = Invalid

diff :: forall a. Ring a => Vector a -> Vector a -> Vector a
diff v1 v2 = v1 `add` (-one `smult` v2)

-- | dot product between two vectors.
-- | returns zero if the two vectors have not the same size
-- | https://en.wikipedia.org/wiki/Dot_product
dot :: forall a. Semiring a => Vector a -> Vector a -> a
dot (Vector v1) (Vector v2)
    | length v1 == length v2 = foldl (+) zero $ zipWith (*) v1 v2
dot _ _ = zero

-- | scalar multiplication
smult :: forall a. Semiring a => a -> Vector a -> Vector a
smult a (Vector v) = Vector $ map (a * _) v
smult _ _ = Invalid

opposite :: forall a. Ring a => Vector a -> Vector a
opposite (Vector v) = Vector $ map (zero - _) v
opposite _ = Invalid

-- | returns true if v1 and v2 are colinear i.e. there exists a scalar n such that v1 = n v2
-- | https://en.wikipedia.org/wiki/Collinearity
colinear :: forall a. Eq a => Field a => Vector a -> Vector a -> Boolean
colinear (Vector v1) (Vector v2) =
    case uncons $ zipWith (/) v1 v2 of
        Nothing -> true
        Just {head, tail} -> all (_ == head) tail
colinear _ _ = false
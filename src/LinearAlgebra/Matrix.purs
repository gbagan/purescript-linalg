module LinearAlgebra.Matrix
  ( Matrix  
  , Solutions
  , fromArray
  , fromFunction
  , fromColumns
  , eye
  , diag
  , mapWithIndex
  , toArray
  , ncols
  , nrows
  , column
  , row
  , columns
  , rows
  , index
  , index'
  , add
  , diff
  , scale
  , mult
  , mult'
  , kronecker
  , transpose
  , inverse
  , kernel
  , image
  , determinant
  , rank
  , trace
  , gaussJordan
  , solveLinearSystem
  , solveLinearSystem'
  ) where

import Prelude hiding (add)
import Data.Array ((..), (!!), all, any, filter, find, foldl, length, replicate, updateAtIndices, zipWith)
import Data.Array as Array
import Data.Function.Uncurried (Fn3, Fn4, mkFn3, runFn3, runFn4)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import LinearAlgebra.Vector as V

newtype Matrix a
  = Matrix { r :: Int, c :: Int, m :: Array (Array a) }

derive instance Eq a => Eq (Matrix a)

instance Functor Matrix where
  map f (Matrix {r, c, m}) = Matrix {r, c, m: map (map f) m}

instance Show a => Show (Matrix a) where
  show (Matrix {r, c, m}) | r == 0 = "Invalid matrix"
                          | otherwise = "(Matrix " <> show r <> " " <> show c <> " " <> show m <> ")"

invalid :: forall a. Matrix a
invalid = Matrix { r: 0, c: 0, m: [] }

fromFunction :: forall a. Int -> Int -> (Int -> Int -> a) -> Matrix a
fromFunction r c f = Matrix { r, c, m: (0 .. (r - 1)) <#> \i -> (0 .. (c - 1)) <#> f i }

fromArray :: forall a. Semiring a => Int -> Int -> Array (Array a) -> Matrix a
fromArray r c m = fromFunction r c \i j -> fromMaybe zero $ m !! i >>= (_ !! j) 

fromColumns :: forall a. Semiring a => Int -> Int -> Array (V.Vector a) -> Matrix a
fromColumns r c a = transpose $ fromArray c r (V.toArray <$> a)

toArray :: forall a. Matrix a -> Array (Array a)
toArray (Matrix { m }) = m

nrows :: forall a. Matrix a -> Int
nrows (Matrix { r }) = r

ncols :: forall a. Matrix a -> Int
ncols (Matrix { c }) = c

-- | returns the element at indices (i, j)
-- | returns zero if the indices are not valid
index :: forall a. Semiring a => Matrix a -> Int ->Int -> a
index m i j = fromMaybe zero $ index' m i j

foreign import _index :: forall a. Fn3 (Matrix a) Int Int a

index' :: forall a. Matrix a -> Int -> Int -> Maybe a
index' (Matrix { m }) i j = m !! i >>= (_ !! j)

-- mapWithIndex is the bottleneck for many algorithms so it is implemented in javascript
foreign import mapWithIndexImpl :: forall a b. Fn4 (Array (Array a)) Int Int (Fn3 Int Int a b) (Array (Array b))

mapWithIndex :: forall a b. (Int -> Int -> a -> b) -> Matrix a -> Matrix b
mapWithIndex f (Matrix { r, c, m }) = Matrix { r, c, m: runFn4 mapWithIndexImpl m r c (mkFn3 f) }

_mapWithIndex :: forall a b. (Fn3 Int Int a b) -> Matrix a -> Matrix b
_mapWithIndex f (Matrix { r, c, m }) = Matrix { r, c, m: runFn4 mapWithIndexImpl m r c f }

row :: forall a. Matrix a -> Int -> V.Vector a
row (Matrix { m }) i = fromMaybe (V.fromArray []) $ V.fromArray <$> m !! i

column :: forall a. Matrix a -> Int -> V.Vector a
column m@(Matrix { r }) j = fromMaybe (V.fromArray []) $ V.fromArray <$> traverse (\i -> index' m i j) (0 .. (r - 1))

rows :: forall a. Matrix a -> Array (V.Vector a)
rows (Matrix { m }) = V.fromArray <$> m

columns :: forall a. Matrix a -> Array (V.Vector a)
columns = rows <<< transpose

-- | computes the identity matrix of dimension nxn
-- | https://en.wikipedia.org/wiki/Identity_matrix
eye :: forall a. Semiring a => Int -> Matrix a
eye n = fromFunction n n \i j -> if i == j then one else zero

diag :: forall a. Semiring a => Array a -> Matrix a
diag v = fromFunction (length v) (length v) \i j -> if i == j then fromMaybe zero (v !! i) else zero

-- | computes the transpose of the matrix
-- | https://en.wikipedia.org/wiki/Transpose
transpose :: forall a. Matrix a -> Matrix a
transpose m@(Matrix { r, c }) = fromFunction c r \i j -> runFn3 _index m j i

add :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
add m1 m2
  | nrows m1 /= nrows m2 || ncols m1 /= ncols m2 = fromArray 0 0  []
  | otherwise = fromFunction (nrows m1) (ncols m1) \i j -> index m1 i j + index m2 i j

diff :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
diff m1 m2 = add m1 (-one `scale` m2)

-- | computes the (left) scalar multiplication of a matrice
-- | https://en.wikipedia.org/wiki/Scalar_multiplication
scale :: forall a. Semiring a => a -> Matrix a -> Matrix a
scale x = map (x * _)

mult :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
mult m1@(Matrix { r: r1, c: c1 }) m2@(Matrix { r: r2, c: c2 })
  | c1 == r2 = fromFunction r1 c2 \i j -> V.dot (row m1 i) (column m2 j)
mult _ _ = invalid

mult' :: forall a. Semiring a => Matrix a -> V.Vector a -> V.Vector a
mult' m@(Matrix { r }) v = V.fromFunction r \i -> V.dot (row m i) v

-- | Kronecker product
-- | https://en.wikipedia.org/wiki/Kronecker_product

kronecker :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
kronecker m@(Matrix {r, c}) m'@(Matrix {r: r', c: c'}) =
  fromFunction (r * r') (c * c') \i j -> index m (i / r') (j / c') * index m' (i `mod` r') (j `mod` c')


mapRow :: forall a. Int -> (a -> a) -> Matrix a -> Matrix a
mapRow k f m = _mapWithIndex (mkFn3 \i _ x -> if i == k then f x else x) m

swapRows :: forall a. Semiring a => Int -> Int -> Matrix a -> Matrix a
swapRows r1 r2 m = _mapWithIndex fn m
  where
  fn = mkFn3 \i j x ->
    if i == r1 then runFn3 _index m r2 j
    else if i == r2 then runFn3 _index m r1 j
    else x

-- | computes the reduced row echelon form of the matrix and compute its determinant if the matrix is square
-- | see https://en.wikipedia.org/wiki/Row_echelon_form 
gaussJordan :: forall a. Eq a => Field a => Matrix a -> { mat :: Matrix a, det :: a }
gaussJordan m@(Matrix { r, c }) = { mat: res.mat, det: res.det }
  where
  res = foldl step { mat: m, pivot: 0, det: one } (0 .. (c - 1))

  step { mat, pivot, det } j = case range pivot (r - 1) # find \i -> index mat i j /= zero of
    Nothing -> { mat, pivot, det: zero }
    Just k ->
      let
        v = index mat k j
        mat2 = mapRow k (_ / v) mat
        mat3 = swapRows k pivot mat2
        mat4 =
          mat3
            # _mapWithIndex (mkFn3 \i j' x ->
                if i == pivot then x else x - (runFn3 _index mat3 i j) * (runFn3 _index mat3 pivot j')
            )
      in
        { mat: mat4
        , pivot: pivot + 1
        , det: det * v * (if pivot == k then one else -one)
        }

  range n n'
    | n <= n' = n .. n'
    | otherwise = []

augmentedMatrix :: forall a. Semiring a => Matrix a -> Matrix a
augmentedMatrix m@(Matrix { r, c }) = fromFunction r (r + c) fAug
  where
  fAug i j
    | j < c = index m i j
    | i == j - c = one
    | otherwise = zero

-- | computes the inverse of the matrix
-- | using Gauss-Jordan Elimination and augmented matrix
-- | https://en.wikipedia.org/wiki/Invertible_matrix#Gaussian_elimination
inverse :: forall a. Eq a => Field a => Matrix a -> Maybe (Matrix a)

inverse m@(Matrix { r, c })
  | r == c =
    if index echelon (r - 1) (r - 1) == one then
      Just $ fromFunction r r \i j -> index echelon i (j + r)
    else
      Nothing
    where
    echelon = _.mat $ gaussJordan $ augmentedMatrix m

inverse _ = Nothing

-- | computes the trace of the matrix
-- | https://en.wikipedia.org/wiki/Trace_(linear_algebra)
trace :: forall a. Eq a => Semiring a => Matrix a -> a
trace m@(Matrix { r, c })
  | r == c = 0 .. (r - 1) # foldl (\acc i -> acc + index m i i) zero

trace _ = zero

-- | computes the determinant of a square matrix
-- | https://en.wikipedia.org/wiki/Determinant
determinant :: forall a. Eq a => Field a => Matrix a -> a
determinant = _.det <<< gaussJordan

filterWithIndex :: forall a. (Int -> a -> Boolean) -> Array a -> Array a
filterWithIndex f t = _.val <$> filtered
  where
  zipped = zipWith { val: _, i: _ } t (0 .. (length t - 1))
  filtered = zipped # filter \{ val, i } -> f i val

imker :: forall a. Eq a => Field a => Matrix a -> { im :: Array (V.Vector a), ker :: Array (V.Vector a) }
imker m@(Matrix { r, c }) = { im, ker }
  where
  echelon = _.mat $ gaussJordan $ augmentedMatrix $ transpose m
  a = fromFunction c r \i j -> index echelon i j
  b = fromFunction c c \i j -> index echelon i (j + r)
  im = rows a # filter (not <<< V.null)
  ker = rows b # filterWithIndex \i _ -> V.null (row a i)

-- | computes a basis the image (or column space) of the matrix
-- | https://en.wikipedia.org/wiki/Row_and_column_spaces
image :: forall a. Eq a => Field a => Matrix a -> Array (V.Vector a)
image = _.im <<< imker

-- | computes a basis for the kernel (or null space) of the matrix
-- | https://en.wikipedia.org/wiki/Kernel_(linear_algebra)
kernel :: forall a. Eq a => Field a => Matrix a -> Array (V.Vector a)
kernel = _.ker <<< imker

-- | computes the rank of the matrix
rank :: forall a. Eq a => Field a => Matrix a -> Int
rank = length <<< _.im <<< imker

-- | Represents the set of solutions for the function solveLinearSystem.
-- | The set of solutions is { sol + v | v is a linear combination of vectors in basis }
type Solutions a
  = { sol :: V.Vector a, basis :: Array (V.Vector a) }

-- | solve the equation M x = b for a given matrix M and vector b
solveLinearSystem :: forall a. Eq a => Field a => Matrix a -> V.Vector a -> Maybe (Solutions a)
solveLinearSystem m b = { sol: _, basis: kernel m } <$> solveLinearSystem' m b

-- | same as solveLinearSystem but without basis
solveLinearSystem' :: forall a. Eq a => Field a => Matrix a -> V.Vector a -> Maybe (V.Vector a)
solveLinearSystem' m@(Matrix { r, c }) b =
  -- if the last non zero row contains is of the form 0 = 1 then there is no solution 
  if r == 0 || (0 .. (c - 1) # all \j -> runFn3 _index echelon (r' - 1) j == zero) then
    Nothing
  else
    let
      toBeUpdated =
        0 .. (r' - 1)
          <#> ( \i ->
                -- k is the column of the leading one of the row i, k always exists
                let
                  k = fromMaybe 0 $ 0 .. (c - 1) # find \j -> runFn3 _index echelon i j == one
                in
                  k /\ runFn3 _index echelon i c
            )

      sol = V.fromArray $ updateAtIndices toBeUpdated (replicate c zero)
    in
      Just sol
  where
  augmented = fromFunction r (c + 1) \i j -> if j == c then V.index b i else index m i j
  echelon = removeZeroRows $ _.mat $ gaussJordan augmented
  r' = nrows echelon

removeZeroRows :: forall a. Eq a => Semiring a => Matrix a -> Matrix a
removeZeroRows (Matrix { c, m }) = Matrix { r: Array.length m', c, m: m' }
  where m' = filter (any (_ /= zero)) m

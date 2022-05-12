module LinearAlgebra.Matrix
  ( Matrix
  , fromArray
  , fromFunction
  , invalid
  , isValid
  , identity
  , ncols
  , nrows
  , elem
  , elem'
  , mapWithIndex
  , row
  , column
  , rows
  , columns
  , transpose
  , add
  , diff
  , smult
  , product
  , inverse
  , gaussJordan
  , determinant
  , image
  , kernel
  , rank
  )
  where

import Prelude
import Data.Array ((..), (!!), all, filter, find, foldl, length, null, uncons, zipWith)
import Data.Array as Array
import Data.Traversable (traverse)
import Data.Maybe (Maybe(..), fromMaybe, fromJust)
import Partial.Unsafe (unsafePartial)
import LinearAlgebra.Vector as V

data Matrix a = Matrix Int Int (Array (Array a)) | Invalid

derive instance Eq a => Eq (Matrix a)

instance Functor Matrix where
    map _ Invalid = Invalid
    map f (Matrix r c m) = Matrix r c $ map (map f) m

instance Show a => Show (Matrix a) where
    show Invalid = "Invalid matrix"
    show (Matrix r c m) = "(Matrix " <> show r <> " " <> show c <> " " <> show m <> ")"

fromFunction :: forall a. Int -> Int -> (Int -> Int -> a) -> Matrix a
fromFunction r c f = Matrix r c $ (0..(r-1)) <#> \i -> (0..(c-1)) <#> \j -> f i j

fromArray :: forall a. Array (Array a) -> Matrix a
fromArray m = case uncons m of
    Just {head, tail} | not (null head) && all (\line -> length line == length head) tail ->
        Matrix (length m) (length head) m
    _ -> Invalid

invalid :: forall a. Matrix a
invalid = Invalid

isValid :: forall a. Matrix a -> Boolean
isValid (Matrix _ _ _) = true
isValid _ = false

nrows :: forall a. Matrix a -> Int
nrows (Matrix r _ _) = r
nrows _ = 0

ncols :: forall a. Matrix a -> Int
ncols (Matrix _ c _) = c
ncols _ = 0

-- | returns the element at indices (i, j)
-- | returns zero if the indices are not valid
elem :: forall a. Semiring a => Int -> Int -> Matrix a -> a
elem i j m = fromMaybe zero $ elem' i j m

unsafeElem :: forall a. Int -> Int -> Matrix a -> a
unsafeElem i j m = unsafePartial $ fromJust $ elem' i j m

elem' :: forall a. Int -> Int -> Matrix a -> Maybe a
elem' i j (Matrix _ _ m) = m !! i >>= (_ !! j)
elem' _ _ Invalid = Nothing

mapWithIndex :: forall a. (Int -> Int -> a -> a) -> Matrix a -> Matrix a
mapWithIndex f (Matrix r c m) = Matrix r c $ m # Array.mapWithIndex \i -> Array.mapWithIndex (f i)
mapWithIndex _ _ = Invalid

row :: forall a. Int -> Matrix a -> V.Vector a
row i (Matrix _ _ m) = fromMaybe V.invalid $ V.fromArray <$> m !! i
row _ _ = V.invalid

column :: forall a. Int -> Matrix a -> V.Vector a
column j m@(Matrix r _ _) = fromMaybe V.invalid $ V.fromArray <$> traverse (\i -> elem' i j m) (0..(r-1))
column _ _ = V.invalid       

rows :: forall a. Matrix a -> Array (V.Vector a)
rows (Matrix _ _ m) = V.fromArray <$> m
rows Invalid = []

columns :: forall a. Matrix a -> Array (V.Vector a)
columns = rows <<< transpose

-- | computes the identity matrix of size n
-- | https://en.wikipedia.org/wiki/Identity_matrix
identity :: forall a. Field a => Int -> Matrix a
identity n = fromFunction n n \i j -> if i == j then one else zero

-- | computes the transpose of the matrix
-- | https://en.wikipedia.org/wiki/Transpose
transpose :: forall a. Matrix a -> Matrix a
transpose m@(Matrix r c _) = fromFunction c r \i j -> unsafeElem j i m
transpose _ = Invalid

-- | computes the addition of two matrices
-- | https://en.wikipedia.org/wiki/Matrix_addition
add :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
add m1 m2 | nrows m1 /= nrows m2 || ncols m1 /= ncols m2 = Invalid
          | otherwise = fromFunction (nrows m1) (ncols m1) \i j -> elem i j m1 + elem i j m2

diff :: forall a. Ring a => Matrix a -> Matrix a -> Matrix a
diff m1 m2 = add m1 (-one `smult` m2)

-- | computes the (left) scalar multiplication of a matrice
-- | https://en.wikipedia.org/wiki/Scalar_multiplication
smult :: forall a. Semiring a => a -> Matrix a -> Matrix a
smult x = map (x * _)

product :: forall a. Semiring a => Matrix a -> Matrix a -> Matrix a
product m1@(Matrix r1 c1 _) m2@(Matrix r2 c2 _)
    | c1 == r2 = fromFunction r1 c2 \i j -> V.dot (row i m1) (column j m2)
product _ _ = Invalid

mapRow :: forall a. Int -> (a -> a) -> Matrix a -> Matrix a 
mapRow k f = mapWithIndex \i _ x -> if i == k then f x else x

swapRows :: forall a. Semiring a => Int -> Int -> Matrix a -> Matrix a 
swapRows r1 r2 m = mapWithIndex fn m where
    fn i j x | i == r1 = elem r2 j m
             | i == r2 = elem r1 j m
             | otherwise = x

-- | computes the reduced row echelon form of the matrix and compute its determinant if the matrix is square
-- | see https://en.wikipedia.org/wiki/Row_echelon_form
gaussJordan :: forall a. Eq a => Field a => Matrix a -> {echelon :: Matrix a, det :: a}
gaussJordan m@(Matrix r c _) = {echelon: res.mat, det: res.det} where
    res = foldl step {mat: m, pivot: 0, det: one} (0..(c-1))
    step {mat, pivot, det} j =
        case range pivot (r-1) # find \i -> elem i j mat /= zero of
            Nothing -> {mat, pivot, det: zero}
            Just k ->
                let v =  elem k j mat
                    mat2 = mapRow k (_ / v) mat
                    mat3 = swapRows k pivot mat2
                    mat4 = mat3 # mapWithIndex \i j' x -> if i == pivot then x else x - (elem i j mat3) * (elem pivot j' mat3)
                in {mat: mat4, pivot: pivot+1, det: det * v * (if pivot == k then one else -one)}
    range n n' | n <= n' = n .. n'
               | otherwise = []
gaussJordan _ = {echelon: Invalid, det: zero}

augmentedMatrix :: forall a. Semiring a => Matrix a -> Matrix a
augmentedMatrix m@(Matrix r c _) = fromFunction r (r + c) fAug where
    fAug i j | j < c = elem i j m
             | i == j - c = one
             | otherwise = zero
augmentedMatrix _ = Invalid

-- | computes the inverse of the matrix
-- | using Gauss-Jordan Elimination and augmented matrix
-- | https://en.wikipedia.org/wiki/Invertible_matrix#Gaussian_elimination
inverse :: forall a. Eq a => Field a => Matrix a -> Matrix a
inverse m@(Matrix r c _) | r == c =
    if (0..(r-1)) # all \i -> elem i i echelon == one then
        fromFunction r r \i j -> elem i (j + r) echelon
    else
        Invalid
    where
    echelon = (gaussJordan $ augmentedMatrix m).echelon
inverse _ = Invalid

-- | computes the determinant of a square matrix
-- | https://en.wikipedia.org/wiki/Determinant
determinant :: forall a. Eq a => Field a => Matrix a -> a
determinant = _.det <<< gaussJordan

filterWithIndex :: forall a. (Int -> a -> Boolean) -> Array a -> Array a
filterWithIndex f t = _.val <$> filtered where
    zipped = zipWith {val: _, index: _} t (0..(length t - 1))
    filtered = zipped # filter \{val, index} -> f index val 

imker :: forall a. Eq a => Field a => Matrix a -> {im :: Array (V.Vector a), ker :: Array (V.Vector a)}
imker m@(Matrix r c _) = {im, ker} where
    echelon = _.echelon $ gaussJordan $ augmentedMatrix $ transpose m
    a = fromFunction c r \i j -> elem i j echelon
    b = fromFunction c c \i j -> elem i (j + r) echelon
    im = rows a # filter (not <<< V.null)
    ker = rows b # filterWithIndex \i _ -> V.null (row i a)
imker _ = {im: [], ker: []}


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
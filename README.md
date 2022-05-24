# purescript-linalg

A linear algebra library written in Purescript

currently supports
- vectors and matrices over any field
- vector: addition, dot product, product by scalar, ...
- matrix: addition, difference, product, transpose, inverse, determinant, kernel, image, ...
- Gauss Jordan elimination
- solves systems of linear equations (returns a solution and a basis if there is an infinite number of solutions)

### Documentation
Documentation is [published on Pursuit](https://pursuit.purescript.org/packages/purescript-linalg)

### Quick start

Install

```spago install linalg```

### Example

Consider the following linear system in $\mathbb{Q}$
- x + 2y = 7
- 3y + 4z = 8
- 5z + 6t = 9

This is equivalent to solve

$$\begin{bmatrix}
  1 & 2 & 0 & 0\\
  0 & 3 & 4 & 0\\
  0 & 0 & 5 & 6
\end{bmatrix} \begin{bmatrix}
           x \\
           y \\
           z \\
           t
\end{bmatrix} = \begin{bmatrix}
      7 \\
      8 \\
      9 
\end{bmatrix}$$

You can solve with LinAlg in that way.

```purescript
module Main where
import Prelude
import Effect.Console (logShow)
import Data.Rational ((%))
import Data.LinearAlgebra.Vector as V
import Data.LinearAlgebra.Matrix as M

main = do
  let m = M.fromArray 3 4 [ [1, 2, 0, 0]
                          , [0, 3, 4, 0]
                          , [0, 0, 5, 6]
                          ] <#> (_ % 1)
  let b = V.fromArray [7, 8, 9] <#> (_ % 1)
  logShow $ M.solveLinearSystem m b
```

and obtain
```Just { basis: [(Vector [1 % 1,-1 % 2,3 % 8,-5 % 16])], sol: (Vector [97 % 15,4 % 15,9 % 5,0 % 1]) }```

That means the solutions are

$$\begin{bmatrix}
     \frac{97}{15} \\
     \frac{4}{15} \\
     \frac{9}{5} \\
     0
   \end{bmatrix} + q \begin{bmatrix}
   1 \\
   -\frac{1}{2} \\
   \frac{3}{8} \\
   -\frac{5}{16}
   \end{bmatrix}$$

for any $q \in \mathbb{Q}$

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

to solve the following linear system in $\mathbb{Q}$
- x + 2y = 7
- 3y + 4z = 8
- 5z + 6t = 9

This is equivalent to solve


You can write something like this

```purescript
module Main where
import Prelude
import Effect.Console (logShow)
import Data.Rational ((%))
import LinearAlgebra.Vector as V
import LinearAlgebra.Matrix as M

main = do
  let m = M.fromArray [ [1%1, 2%1, 0%1, 0%1]
                      , [0%1, 3%1, 4%1, 0%1]
                      , [0%1, 0%1, 5%1, 6%1]
                      ]
  let b = V.fromArray [7%1, 8%1, 9%1]
  logShow $ solveLinearSystem m b
```

and obtain
```Just { basis: [(Vector [1 % 1,-1 % 2,3 % 8,-5 % 16])], sol: (Vector [97 % 15,4 % 15,9 % 5,0 % 1]) }```

That means that the set of solutions is { [97/15, 4/15, 9/5, 0] + q * [1; -1/2; 3/8; -5/16] | q in Q }

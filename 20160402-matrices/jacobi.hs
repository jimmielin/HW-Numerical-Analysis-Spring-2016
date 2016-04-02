-- Jacobi Iterative Method for solving Linear Equation System
-- April 2, 2016
import Data.Matrix

-- |Jacobi Iterative Method for solving Linear Equation System Ax=b
jacobisolve :: (RealFloat a, Enum a) => Matrix a                      -- ^ A square matrix with **nonzero** diagonals
                                     -> Matrix a                      -- ^ b column vector, matrix form
                                     -> Int                           -- ^ number of iterations applied
                                     -> Matrix a                      -- ^ solution x
jacobisolve a b n = jacobisolve_int j (zero (nrows a) 1) dib n where
    (di, l, u) = diludecomp a
    j = multStd2 di $ l + u
    dib = multStd2 di b

diludecomp :: (RealFloat a, Enum a) => Matrix a                       -- ^ A square matrix with **nonzero** diagonals
                                    -> (Matrix a, Matrix a, Matrix a) -- ^ (D-1, L, U) matrices
diludecomp m = (di, l, u) where
    n  = nrows m
    di = matrix n n (\(x, y) -> if x == y then 1 / (m ! (x, x)) else 0)
    l  = matrix n n (\(x, y) -> if x > y then m ! (x, y) * (-1) else 0)
    u  = matrix n n (\(x, y) -> if x < y then m ! (x, y) * (-1) else 0)

jacobisolve_int :: (RealFloat a, Enum a) => Matrix a                  -- ^ Jacobi Iterative Matrix J = D^(-1) * (L + U)
                                         -> Matrix a                  -- ^ x_i row vector (matrix form) for solution estimation
                                         -> Matrix a                  -- ^ D^(-1) * b
                                         -> Int                       -- ^ number of runs remaining.
                                         -> Matrix a                  -- ^ x_i row vector solution estimation
jacobisolve_int _ x _ 0   = x
jacobisolve_int j x dib n = jacobisolve_int j xn dib $ if xn == x then 0 else n - 1 where
    xn = (+) dib $ multStd2 j x
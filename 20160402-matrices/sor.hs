-- Successive relaxation (SR) Iterative Method for solving Linear Equation System
-- April 4, 2016
import Data.Matrix

-- |SOR/SUR Iterative Method for solving Linear Equation System Ax=b
--  Note that the Relaxation Factor has to be chosen by the user and this will alter the convergence
--  of the iterative algorithm. Common rules are that, if A is a positive-definite symmetric matrix, then
--  the Gauss-Seidel (\omega = 1) method or a SOR method with (\omega\in (0, 2)) is convergent.
--  If A is a matrix that is strictly diagonally dominant, then Jacobi, G-S, SOR iterations are all convergent,
--  with \omega\in (0, 1)
--  WARNING: Convergence is *NOT* verified.
srsolve :: (RealFloat a, Enum a) => Matrix a                      -- ^ A square matrix with **nonzero** diagonals
                                 -> Matrix a                      -- ^ b column vector, matrix form
                                 -> Int                           -- ^ number of iterations applied
                                 -> a                             -- ^ Successive Relaxation Factor
                                 -> Matrix a                      -- ^ solution x
srsolve a b n w = srsolve_int lw (zero (nrows a) 1) wdlwib n where
    (d, l, u) = dludecomp a
    dlwi   = invl $ (-) d $ fmap (*w) l
    lw     = multStd2 dlwi $ (+) (fmap (*(1 - w)) d) (fmap (*w) u)
    wdlwib = multStd2 (fmap (*w) dlwi) b

dludecomp :: (RealFloat a, Enum a) => Matrix a                       -- ^ A square matrix with **nonzero** diagonals
                                   -> (Matrix a, Matrix a, Matrix a) -- ^ (D, -L, -U) matrices
{-# INLINE dludecomp #-}
dludecomp m = (d, l, u) where
    n  = nrows m
    d  = matrix n n (\(x, y) -> if x == y then m ! (x, x) else 0)
    l  = matrix n n (\(x, y) -> if x > y  then m ! (x, y) * (-1) else 0)
    u  = matrix n n (\(x, y) -> if x < y  then m ! (x, y) * (-1) else 0)

srsolve_int :: (RealFloat a, Enum a) => Matrix a                  -- ^ SOR Iterative Matrix L_w = (D - wL)^(-1) * [(1 - w)D + wU]
                                     -> Matrix a                  -- ^ x_i row vector (matrix form) for solution estimation
                                     -> Matrix a                  -- ^ w (D - wL)^(-1) * b
                                     -> Int                       -- ^ number of runs remaining.
                                     -> Matrix a                  -- ^ x_i row vector solution estimation
srsolve_int _ x _ 0    = x
srsolve_int l x wdlwib n = srsolve_int l xn wdlwib $ if xn == x then 0 else n - 1 where
    xn = (+) wdlwib $ multStd2 l x

-- | Lower Triangular Matrix Forward-Substitution Method for Inverse
invl :: (RealFloat a, Enum a) => Matrix a -> Matrix a
invl m = foldl (\acc x -> (<->) acc $ fmap (/(m ! (x, x))) $ (idv x) - (foldr (\y acc2 -> acc2 + fmap (*(m ! (x, y))) (rowVector $ getRow y acc)) (zero 1 n) [1..(x-1)])) (fmap (/(m ! (1, 1))) $ idv 1) [2..n] where
    n = nrows m
    idv x = matrix 1 n (\t -> if snd t == x then 1 else 0)
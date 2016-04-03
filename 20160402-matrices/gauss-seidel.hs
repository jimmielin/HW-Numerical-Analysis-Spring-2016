-- Gauss-Seidel Iterative Method for solving Linear Equation System
-- April 3, 2016
import Data.Matrix

-- |Gauss-Seidel Iterative Method for solving Linear Equation System Ax=b
gssolve :: (RealFloat a, Enum a) => Matrix a                      -- ^ A square matrix with **nonzero** diagonals
                                 -> Matrix a                      -- ^ b column vector, matrix form
                                 -> Int                           -- ^ number of iterations applied
                                 -> Matrix a                      -- ^ solution x
gssolve a b n = gssolve_int g (zero (nrows a) 1) dlib n where
    (d, l, u) = dludecomp a
    dli  = invl $ d - l
    g    = multStd2 dli u
    dlib = multStd2 dli b

dludecomp :: (RealFloat a, Enum a) => Matrix a                       -- ^ A square matrix with **nonzero** diagonals
                                   -> (Matrix a, Matrix a, Matrix a) -- ^ (D, -L, -U) matrices
dludecomp m = (d, l, u) where
    n  = nrows m
    d = matrix n n  (\(x, y) -> if x == y then m ! (x, x) else 0)
    l  = matrix n n (\(x, y) -> if x > y then m ! (x, y) * (-1) else 0)
    u  = matrix n n (\(x, y) -> if x < y then m ! (x, y) * (-1) else 0)

gssolve_int :: (RealFloat a, Enum a) => Matrix a                  -- ^ Gauss-Seidel Iterative Matrix G = (D - L)^(-1) * U
                                     -> Matrix a                  -- ^ x_i row vector (matrix form) for solution estimation
                                     -> Matrix a                  -- ^ (D - L)^(-1) * b
                                     -> Int                       -- ^ number of runs remaining.
                                     -> Matrix a                  -- ^ x_i row vector solution estimation
gssolve_int _ x _ 0    = x
gssolve_int g x dlib n = gssolve_int g xn dlib $ if xn == x then 0 else n - 1 where
    xn = (+) dlib $ multStd2 g x


-- | Lower Triangular Matrix Forward-Substitution Method for Inverse
invl :: (RealFloat a, Enum a) => Matrix a -> Matrix a
invl m = foldl (\acc x -> (<->) acc $ fmap (/(m ! (x, x))) $ (idv x) - (foldr (\y acc2 -> acc2 + fmap (*(m ! (x, y))) (rowVector $ getRow y acc)) (zero 1 n) [1..(x-1)])) (fmap (/(m ! (1, 1))) $ idv 1) [2..n] where
    n = nrows m
    idv x = matrix 1 n (\t -> if snd t == x then 1 else 0)
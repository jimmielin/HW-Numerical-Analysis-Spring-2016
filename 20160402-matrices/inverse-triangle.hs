-- Back/Forward-subsitution method for Triangular Matrix Inversion
-- April 4, 2016 Midnight
import Data.Matrix

-- | Lower Triangular Matrix Forward-Substitution Method for Inverse
invl :: (RealFloat a, Enum a) => Matrix a -> Matrix a
invl m = foldl (\acc x -> (<->) acc $ fmap (/(m ! (x, x))) $ (idv x) - (foldr (\y acc2 -> acc2 + fmap (*(m ! (x, y))) (rowVector $ getRow y acc)) (zero 1 n) [1..(x-1)])) (fmap (/(m ! (1, 1))) $ idv 1) [2..n] where
    n = nrows m
    idv x = matrix 1 n (\t -> if snd t == x then 1 else 0)
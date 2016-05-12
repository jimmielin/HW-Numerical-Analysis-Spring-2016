-- Inverse Iteration Method for determining eigenvector
-- May 8, 2016
import Data.Matrix
import Data.Maybe


inviter' :: (RealFloat a) => Matrix a                -- matrix A's INVERSE
                          -> Matrix a                -- first (unit) approximation of eigen-vector
                          -> Int                     -- remaining iteration count
                          -> Matrix a                -- approximation of eigen-vector through InvItr Method
inviter' _ z 0 = z
inviter' ai z ns = inviter' ai zs (ns - 1) where
    lencm cm = sqrt $ foldl (\acc x -> acc + x^2) 0 cm
    unitcm cm = fmap (* (1 /(lencm cm))) cm
    zs = unitcm (multStd2 ai z)
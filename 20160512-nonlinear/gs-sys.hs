-- Gauss-Seidel's Method (Nonlinear Equation System Solution)
-- May 13th, 2016
import Data.Matrix

gsroots :: (RealFloat a) => (Matrix (Matrix a -> a))     -- F(x) vector
                         -> (Matrix a)                   -- initial state
                         -> a                            -- maximum epsilon
                         -> ((a -> a) -> a -> a -> a)    -- given solver (f, x0, emax)
                         -> (Matrix a)                   -- result
gsroots f x0 e sv = if abs (maximum (x1 - x0)) > e then jacobiroots f x1 e sv else x1 where
    x1 = foldl (\acc x -> ) [1..(nrows x0)] x0
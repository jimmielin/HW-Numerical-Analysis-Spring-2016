-- Jacobi's Method (Nonlinear Equation System Solution)
-- May 13th, 2016
import Data.Matrix

jacobiroots :: (RealFloat a) => (Matrix (Matrix a -> a))     -- F(x) vector
                             -> (Matrix a)                   -- initial state
                             -> a                            -- maximum epsilon
                             -> ((a -> a) -> a -> a -> a)    -- given solver (f, x0, emax)
                             -> (Matrix a)                   -- result
jacobiroots f x0 e sv = if abs (maximum (x1 - x0)) > e then jacobiroots f x1 e sv else x1 where
   n  = nrows x0
   x1 = foldl (\acc x -> setElem (sv (\y -> (f ! (x, 1)) $ matrix n 1 (\(z, _) -> if z /= x then x0 ! (z, 1) else y)) (x0 ! (x, 1)) e) (x, 1) acc) (matrix n 1 (\_ -> 0)) [1..n]

-- F(x) is formatted as follows
-- fns = fromList 2 1 [(\[x, y] -> x + y) . toList, (\[x, y] -> x - 2*y) . toList]
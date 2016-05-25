-- Jacobi's Method (Nonlinear Equation System Solution)
-- May 13th, 2016
import Data.Matrix
import Data.Complex

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

secroot :: (RealFloat a) => (a -> a)                -- f(x)
                         -> a                       -- initial state 1
                         -> a                       -- initial state 2
                         -> a                       -- maximum epsilon
                         -> a                       -- result
secroot f x1 x2 e = if abs ((-) x3 x2) > e then secroot f x2 x3 e else x3 where
    f1   = f x1
    f2   = f x2
    fpi  = if f2 /= f1 then (/) (x2 - x1) (f2 - f1) else 0
    x3   = if f2 /= f1 then x2 - f2 * fpi           else x2 + e -- reseed


-- F(x) is formatted as follows
-- fns = fromList 2 1 [(\[x, y] -> x + y) . toList, (\[x, y] -> x - 2*y) . toList]
-- complex counterparts
jacobiroots_c :: (RealFloat a) => (Matrix (Matrix (Complex a) -> (Complex a)))     -- F(x) vector
                             -> (Matrix (Complex a))                   -- initial state
                             -> (Complex a)                            -- maximum epsilon
                             -> (((Complex a) -> (Complex a)) -> (Complex a) -> (Complex a) -> (Complex a))    -- given solver (f, x0, emax)
                             -> (Matrix (Complex a))                   -- result
jacobiroots_c f x0 e sv = if (maximum (fmap magnitude (x1 - x0))) > magnitude e then jacobiroots_c f x1 e sv else x1 where
   n  = nrows x0
   x1 = foldl (\acc x -> setElem (sv (\y -> (f ! (x, 1)) $ matrix n 1 (\(z, _) -> if z /= x then x0 ! (z, 1) else y)) (x0 ! (x, 1)) e) (x, 1) acc) (matrix n 1 (\_ -> 0)) [1..n]

secroot_c :: (RealFloat a) => (Complex a -> Complex a)                -- f(x)
                         -> Complex a                       -- initial state 1
                         -> Complex a                       -- initial state 2
                         -> Complex a                       -- maximum epsilon (real, Complex for type only)
                         -> Complex a                       -- result
secroot_c f x1 x2 e = if magnitude ((-) x3 x2) > magnitude e then secroot_c f x2 x3 e else x3 where
    f1   = f x1
    f2   = f x2
    fpi  = if f2 /= f1 then (/) (x2 - x1) (f2 - f1) else 0
    x3   = if f2 /= f1 then x2 - f2 * fpi           else x2 + e -- reseed

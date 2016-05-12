-- Secant Method (Nonlinear Equation Solution)
-- May 13th, 2016

secroot :: (RealFloat a) => (a -> a)                -- f(x)
                         -> a                       -- initial state 1
                         -> a                       -- initial state 2
                         -> a                       -- maximum epsilon
                         -> a                       -- result
secroot f x1 x2 e = if abs ((-) x3 x2) > e then secroot f x2 x3 e else x3 where
    f1   = f x1
    f2   = f x2
    fpi  = if f2 /= f1 then (/) (x2 - x1) (f2 - f1) else error "derivative singular point in iteration!"
    x3   = x2 - f2 * fpi
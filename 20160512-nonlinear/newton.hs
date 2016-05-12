-- Newton Approximation (Nonlinear Equation Solution)
-- May 8th, 2016, improved May 12th, 2016

newroot :: (RealFloat a) => (a -> a)                -- f(x)
                         -> (a -> a)                -- f'(x)
                         -> a                       -- initial state
                         -> a                       -- maximum epsilon
                         -> a                       -- result
newroot f fp xn e = if abs ((-) xnx xn) > e then newroot f fp xnx e else xnx where
    fn  = f xn
    fpn = fp xn
    xnx = if fpn /= 0 then (-) xn (fn / fpn) else error "derivative singular point in iteration!"
    -- todo: maybe solve singular point issue by adding epsilon?
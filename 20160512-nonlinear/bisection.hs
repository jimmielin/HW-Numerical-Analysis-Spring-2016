-- Bisection Method for solving non-linear equation
-- May 12th, 2016

bisroot :: (RealFloat a, Show a) => (a -> a)                -- f(x)
                         -> a                       -- left bound
                         -> a                       -- right bound
                         -> a                       -- maximum epsilon
                         -> a                       -- result
bisroot f a b e = 
    if (f b) * (f a) > 0 
        then error "there doesn't seem to be a bisection solvable root here"
        else if (b - a) < e then m else bisroot f a' b' e where
    m  = (b + a) / 2
    d  = (f m) * (f a)
    a' = if d > 0 then m else a
    b' = if d > 0 then b else m

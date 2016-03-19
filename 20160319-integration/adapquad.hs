-- Adaptive Quadrature Numerical Integration Method
-- March 19, 2016

-- |Adaptive Quadrature Numerical Integration, using Trapezoids
adapqt :: (RealFloat a, Enum a) => (a -> a)                     -- ^ the to-be-integrated function
                                -> a                            -- ^ integrate from where
                                -> a                            -- ^ ...to where
                                -> a                            -- ^ maximum \epsilon
                                -> a                            -- ^ the result.
adapqt f a b e = if e <= 0 then error "epsilon > 0" else adapqt_int f a b e $ b - a

-- |Internal Termination Function for AQNI, Trapezoids Variant
adapqt_int :: (RealFloat a, Enum a) => (a -> a)                 -- ^ the to-be-integrated function
                                    -> a                        -- ^ integrate from where
                                    -> a                        -- ^ ...to where
                                    -> a                        -- ^ maximum \epsilon
                                    -> a                        -- ^ current step-size
                                    -> a                        -- ^ the result.
adapqt_int f a b e h = if (th - thalf) < e then thalf else adapqt_int f a b e $ h/2 where
    th = adapqt_intt f a b h
    thalf = adapqt_intt f a b $ h / 2

-- |Internal Calculation Function for AQNI, Trapezoids Variant
adapqt_intt :: (RealFloat a, Enum a) => (a -> a)                -- ^ the to-be-integrated function
                                     -> a                       -- ^ integrate from where
                                     -> a                       -- ^ ...to where
                                     -> a                       -- ^ current step-size
                                     -> a                       -- ^ the result.
adapqt_intt f a b h = h/2 * sum fsapp where
    fsapp = [f (a + i*h) + f (a + (i+1)*h) | i <- [0..((b - a)/h - 1)]]
    -- some people love haskell for their syntactic sugar, but it should
    -- not be like this.
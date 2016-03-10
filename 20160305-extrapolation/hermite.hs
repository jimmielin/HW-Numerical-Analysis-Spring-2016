runge :: (RealFloat a) => a -> a
runge x = 1/(1+x*x)

rungedrv :: (RealFloat a) => a -> a
rungedrv x = -2 * x/(1+x^2)^2

aleft :: (RealFloat a) => a -> a -> a -> a  -- (x; a, b) is aleft x a b
aleft  a b x = (1 + 2*(x-a)/(b-a)) * ((x-b)/(a-b))^2

aright :: (RealFloat a) => a -> a -> a -> a
aright a b x = (1 + 2*(x-b)/(a-b)) * ((x-a)/(a-b))^2

bleft :: (RealFloat a) => a -> a -> a -> a
bleft  a b x = (x-a)*((x-b)/(a-b))^2

bright :: (RealFloat a) => a -> a -> a -> a
bright a b x = (x-b)*((x-a)/(a-b))^2

-- usage: h3 f fp a b. f is function, fp is its derivative function, a, b are two ends of the 3-order approximation
h3 :: (RealFloat a) => (a -> a) -> (a -> a) -> a -> a -> (a -> a)
h3 f fp a b = (\x -> (f a) * (aleft a b x) + (f b) * (aright a b x) + (fp a) * (bleft a b x) + (fp b) * (bright a b x))

poly :: (RealFloat a) => (a -> a)
poly x
    | x < -5  = error "out of range"
    | x <= -4 = h3 runge rungedrv (-5) (-4) x
    | x <= -3 = h3 runge rungedrv (-4) (-3) x
    | x <= -2 = h3 runge rungedrv (-3) (-2) x
    | x <= -1 = h3 runge rungedrv (-2) (-1) x
    | x <=  0 = h3 runge rungedrv (-1) (0) x
    | x <=  1 = h3 runge rungedrv ( 0) (1) x
    | x <=  2 = h3 runge rungedrv ( 1) (2) x
    | x <=  3 = h3 runge rungedrv ( 2) (3) x
    | x <=  4 = h3 runge rungedrv ( 3) (4) x
    | x <=  5 = h3 runge rungedrv ( 4) (5) x
    | x >   5 = error "out of range"

    -- ghci:
    -- putStr $ foldl (\acc x -> ((show $ poly (x/10)) ++ ['\n']) ++ acc) "" [-50..50]
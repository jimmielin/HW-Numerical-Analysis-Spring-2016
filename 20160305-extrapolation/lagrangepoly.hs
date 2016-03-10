runge :: Double -> Double
runge x = 1/(1+x*x)

cp :: Double -> Double
cp x = 5 * cos ((2*x + 1)/42 * pi)

proddiffn :: Double -> [Double] -> Double -> Double
proddiffn _ []     _ = 1
proddiffn x (y:ys) g = (if y == g then 1 else (x - y)) * (proddiffn x ys g)

lgrpolya :: (Double -> Double) -> [Double] -> Double -> (Double -> Double)
lgrpolya f xs g = (\y -> (f g) * (proddiffn y xs g) / (proddiffn g xs g))

lgrpoly :: (Double -> Double) -> [Double] -> (Double -> Double)
lgrpoly f xs = foldl (\fn x -> (\y -> fn y + lgrpolya runge xs x y)) (\_ -> 0) xs

polyFD :: (Double -> Double)
polyFD x
    | x < -5  = error "out of range"
    | x <= -4 = lgrpoly runge [-5, -4] x
    | x <= -3 = lgrpoly runge [-4, -3] x
    | x <= -2 = lgrpoly runge [-3, -2] x
    | x <= -1 = lgrpoly runge [-2, -1] x
    | x <=  0 = lgrpoly runge [-1,  0] x
    | x <=  1 = lgrpoly runge [ 0,  1] x
    | x <=  2 = lgrpoly runge [ 1,  2] x
    | x <=  3 = lgrpoly runge [ 2,  3] x
    | x <=  4 = lgrpoly runge [ 3,  4] x
    | x <=  5 = lgrpoly runge [ 4,  5] x
    | x >   5 = error "out of range"
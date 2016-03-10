runge :: Double -> Double
runge x = 1/(1+x*x)

divdiff :: (Double -> Double) -> [Double] -> Double
divdiff f []  = error "empty list"
divdiff f [x] = f x
divdiff f xs  = (divdiff f (tail xs) - divdiff f (init xs)) / (last xs - head xs) 
-- optimize divdiff??

--newtonpoly :: (Double -> Double) -> [Double] -> [Double] -> (Double -> Double)
--newtonpoly _ [] _ r = r
--newtonpoly f (x:xs) xds r = (\x -> (r x) + (divdiff ))
proddiff :: Double -> [Double] -> Double
proddiff _ []   = 1
proddiff x (y:ys) = (x - y) * (proddiff x ys)

newtonpolya :: (Double -> Double) -> [Double] -> Double -> (Double -> Double)
newtonpolya f xds x = (\y -> (divdiff f (x:xds))*(proddiff y (xds)))

-- fst $ foldl (\(fn, xds) x -> ((\y -> fn y + newtonpolya runge xds x y), x:xds)) ((\_ -> 0), []) [1, 2, 3, 4]
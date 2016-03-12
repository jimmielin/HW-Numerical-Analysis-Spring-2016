import Data.Matrix

-- |Tridiagonal matrix algorithm (Thomas algorithm) implementation
solvetri :: (RealFloat a) => Matrix a                 -- ^ the tridiagonal metrix m
                          -> Matrix a                 -- ^ the column-vector matrix d
                          -> Matrix a                 -- ^ returns the solution for the linear equation system MX `eq` d
solvetri m d = fromList (nrows m) 1 $ foldr (\x acc -> ((ds ! (nrows m - length acc, 1)) - (ms ! (nrows m - length acc, nrows m - length acc + 1)) * (head acc)):acc) [ds ! (nrows m, 1)] [1..(nrows m - 1)] where
    (ms, ds) = thomastransform m d

thomastransform :: (RealFloat a) => Matrix a -> Matrix a -> (Matrix a, Matrix a)
thomastransform m d = (ms, thomastransform_intd ms d 1 $ nrows m - 1) where
    ms = thomastransform_intc m 1 $ nrows m - 1

thomastransform_intc :: (RealFloat a) => Matrix a -> Int -> Int -> Matrix a
thomastransform_intc ms n 0 = ms
thomastransform_intc m  1 r = thomastransform_intc (setElem ((m ! (1, 2)) / (m ! (1, 1))) (1, 2) m) 2 (r - 1)
thomastransform_intc ms n r = thomastransform_intc (setElem ((ms ! (n, n+1)) / (ms ! (n, n) - ms ! (n, n-1) * ms ! (n-1, n))) (n, n+1) ms) (n + 1) (r - 1)

thomastransform_intd :: (RealFloat a) => Matrix a -> Matrix a -> Int -> Int -> Matrix a
thomastransform_intd ms ds n (-1) = ds
thomastransform_intd ms d  1 r = thomastransform_intd ms (setElem ((d ! (1, 1)) / (ms ! (1, 1))) (1, 1) d) 2 $ r - 1
thomastransform_intd ms ds n r = thomastransform_intd ms (setElem ((ds ! (n, 1) - (ms ! (n, n-1)) * (ds ! (n - 1, 1))) / (ms ! (n, n) - (ms ! (n, n - 1)) * (ms ! (n - 1, n)))) (n, 1) ds) (n + 1) $ r - 1

-- Hermite Base Functions
aleft :: (RealFloat a) => a -> a -> a -> a  -- (x; a, b) is aleft x a b
aleft  a b x = (1 + 2*(x-a)/(b-a)) * ((x-b)/(a-b))^2

aright :: (RealFloat a) => a -> a -> a -> a
aright a b x = (1 + 2*(x-b)/(a-b)) * ((x-a)/(a-b))^2

bleft :: (RealFloat a) => a -> a -> a -> a
bleft  a b x = (x-a)*((x-b)/(a-b))^2

bright :: (RealFloat a) => a -> a -> a -> a
bright a b x = (x-b)*((x-a)/(a-b))^2

{--
ah :: (RealFloat a) => Int -> a -> a -> a -> Int -> a -> a
ah i a b c n x
    | i == 0 =    if x < b then 0 else (if x < c then aleft a b x else 0)
    | i == n =    if x < a then 0 else (if x <= b then aright a b x else 0)
    | x < a  =    0
    | x < b  =    aright a b x
    | x < c  =    aleft  b c x
    | otherwise = 0

bh :: (RealFloat a) => Int -> a -> a -> a -> Int -> a -> a
bh i a b c n x
    | i == 0 =    if x < b then 0 else (if x < c then bleft a b x else 0)
    | i == n =    if x < a then 0 else (if x <= b then bright a b x else 0)
    | x < a  =    0
    | x < b  =    bright a b x
    | x < c  =    bleft  b c x
    | otherwise = 0
--}

-- alternate implementations for ah, bh as the above seem buggy
ah' :: (RealFloat a) => Int -> Int -> [a] -> a -> a
ah' i n xs x
    | i == 0 = if x < xs !! 0 then 0 else (if x <= xs !! 1 then aleft (xs !! 0) (xs !! 1) x else 0)
    | i == n = if x < xs !! (n - 1) then 0 else (if x <= xs !! n then aright (xs !! (n-1)) (xs !! n) x else 0)
    | otherwise = if x < (xs !! (i - 1)) then 0 else (if x <= (xs !! i) then aright (xs !! (i - 1)) (xs !! i) x else (if x <= (xs !! i + 1) then aleft (xs !! i) (xs !! (i + 1)) x else 0))

bh' :: (RealFloat a) => Int -> Int -> [a] -> a -> a
bh' i n xs x
    | i == 0 = if x < xs !! 0 then 0 else if x <= xs !! 1 then bleft (xs !! 0) (xs !! 1) x else 0
    | i == n = if x < xs !! (n - 1) then 0 else if x <= xs !! n then bright (xs !! (n-1)) (xs !! n) x else 0
    | otherwise = if x < (xs !! (i - 1)) then 0 else if x <= (xs !! i) then bright (xs !! (i - 1)) (xs !! i) x else if x <= (xs !! i + 1) then bleft (xs !! i) (xs !! (i + 1)) x else 0

-- generator functions
genLd :: (RealFloat a) => [a] -> [a]
genLd hs = reverse $ 0 : tail (foldl (\acc x -> ((hs !! (length acc - 1)) / ((hs !! (length acc - 1)) + x)) : acc) [1] hs)

genH :: (RealFloat a) => [a] -> [a]
genH xs = foldr (\x acc -> if length acc + 1 < length xs then (x - (xs !! (length xs - length acc - 2))) : acc else acc) [] xs

genMu :: (RealFloat a) => [a] -> [a] -> [a] -> [a]
genMu lds hs ys = (3*(ys !! 1 - ys !! 0) / hs !! 0) : (foldl (\acc x -> 3*( (1 - x)/(hs !! (length acc - 1))*(ys !! length acc - ys !! (length acc - 1)) + x / (hs !! length acc) * (ys !! (length acc + 1) - ys !! length acc)) : acc) [3*(last ys - last (init ys)) / last hs] (init (tail lds)))

genEqns :: (RealFloat a) => [a]                     -- ^ The list of x coordinates
                         -> [a]                     -- ^ The corresponding list of the y coordinates
                         -> (Matrix a, Matrix a)    -- ^ The tuple (M, D) for the system of equations Mx `eq` D
genEqns xs ys = (matrix (length ys) (length ys) (\(x, y) -> if x == y then 2 else (if y == x + 1 then lds !! (x - 1) else (if y == x - 1 then 1 - lds !! (x - 1) else 0))), fromList (length ys) 1 mus) where -- stub
    hs  = genH  xs
    lds = genLd hs
    mus = genMu lds hs ys

genPoly :: (RealFloat a) => [a]                 -- ^ The list of x coordinates
                         -> [a]                 -- ^ The corresponding list of the y coordinates
                         -> (a -> a)            -- ^ The corresponding 3-order spline polynomial in natural restriction
genPoly xs ys = (\x -> sum $ foldl (\acc y -> (ys !! length acc) * (ah' (length acc) n xs x) + (ms !! length acc) * (bh' (length acc) n xs x) : acc) [] ms) where
    n    = length ys
    eqns = genEqns xs ys
    ms   = toList $ solvetri (fst eqns) (snd eqns)

{--
genPolyDebug xs ys = (xs, ys, n, eqns, ms) where
    n    = length ys
    eqns = genEqns xs ys
    ms   = toList $ solvetri (fst eqns) (snd eqns)

genPolyDebug2 xs ys x = (fd, sum fd) where
    n    = length ys
    eqns = genEqns xs ys
    ms   = toList $ solvetri (fst eqns) (snd eqns)
    fd   = foldl (\acc y -> (ys !! length acc) * (ah' (length acc) n xs x) + (ms !! length acc) * (bh' (length acc) n xs x) : acc) [] ms
--}

-- runge specific (serves as an example I guess?)
runge :: (RealFloat a) => a -> a
runge x = 1/(1+x*x)

poly :: Double -> Double
poly = genPoly [-5..5] $ fmap runge [-5..5]
import Data.Matrix

-- Tridiagonal matrix algorithm (Thomas algorithm) implementation
-- Given a tridiagonal matrix m, and a column-vector matrix d, solves the linear equation system mX=d for column-vector matrix X.

solvetri :: (RealFloat a) => Matrix a -> Matrix a -> Matrix a
solvetri m d = fromList (nrows m) 1 $ foldr (\x acc -> ((ds ! (nrows m - length acc, 1)) - (ms ! (nrows m - length acc, nrows m - length acc + 1)) * (last acc)):acc) [ds ! (nrows m, 1)] [1..(nrows m - 1)] where
    (ms, ds) = thomastransform m d

thomastransform :: (RealFloat a) => Matrix a -> Matrix a -> (Matrix a, Matrix a)
thomastransform m d = (ms, thomastransform_intd ms d 1 $ nrows m - 1) where
    ms = thomastransform_intc m 1 $ nrows m - 1

thomastransform_intc :: (RealFloat a) => Matrix a -> Int -> Int -> Matrix a
thomastransform_intc ms n 0 = ms
thomastransform_intc m  1 r = thomastransform_intc (setElem ((m ! (1, 2)) / (m ! (1, 1))) (1, 2) m) 2 (r - 1)
thomastransform_intc ms n r = thomastransform_intc (setElem ((ms ! (n, n+1)) / (ms ! (n, n) - ms ! (n, n-1) * ms ! (n-1, n))) (n, n+1) ms) (n + 1) (r - 1)

thomastransform_intd :: (RealFloat a) => Matrix a -> Matrix a -> Int -> Int -> Matrix a
thomastransform_intd ms ds n 0 = ds
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

ah :: (RealFloat a) => Int -> a -> a -> a -> a -> a -> a
ah i a b c n x
    | i == 0 =    if x < a then 0 else (if x < b then aleft a b x else 0)
    | x < a =     0
    | x < b =     aright a b x
    | x < c =     aleft  b c x
    | otherwise = 0

bh :: (RealFloat a) => Int -> a -> a -> a -> a -> a -> a
bh i a b c n x
    | i == 0 =    if x < a then 0 else (if x < b then bleft a b x else 0)
    | x < a =     0
    | x < b =     bright a b x
    | x < c =     bleft  b c x
    | otherwise = 0

genLd :: (RealFloat a) => [a] -> [a]
genLd hs = reverse $ 0 : tail (foldl (\acc x -> ((hs !! (length acc - 1)) / ((hs !! (length acc - 1)) + x)) : acc) [1] hs)

genH :: (RealFloat a) => [a] -> [a]
genH xs = foldr (\x acc -> if length acc + 1 < length xs then (x - (xs !! (length xs - length acc - 2))) : acc else acc) [] xs

genMu :: (RealFloat a) => [a] -> [a] -> [a] -> [a]
genMu lds hs ys = (3*(ys !! 1 - ys !! 0) / hs !! 0) : (foldl (\acc x -> 3*( (1 - x)/(hs !! (length acc - 1))*(ys !! length acc - ys !! (length acc - 1)) + x / (hs !! length acc) * (ys !! (length acc + 1) - ys !! length acc)) : acc) [3*(last ys - last (init ys)) / last hs] (init (tail lds)))

genEqns :: (RealFloat a) => [a] -> [a] -> (Matrix a, Matrix a)
genEqns xs ys = (matrix (length ys) (length ys) (\(x, y) -> 1), fromList (length ys) 1 mus) where -- stub
    hs  = genH  xs
    lds = genLd hs
    mus = genMu lds hs ys

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

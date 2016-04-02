-- some bootstrap code for ex. 4-3
let epsilon = 1
let a = 0.5
let n = 100
let h = 1/n
let matrixa = matrix (n-1) (n-1) (\(x, y) -> if x == y then (-1) * (2 * epsilon + h) else if x == y - 1 then epsilon + h else if x == y + 1 then epsilon else 0)
let matrixb = matrix (n - 1) 1 (\(r, _) -> if r /= n - 1 then a * h * h else a * h * h - epsilon - h)

let exact :: Double -> Double
exact x = (1 - a) * (1 - exp((-1) * x / epsilon)) / (1 - exp((-1)/epsilon)) + a * x
putStr $ foldl (\acc x -> acc ++ ['\n'] ++ show (exact $ x/n)) "" [1..(n-1)]

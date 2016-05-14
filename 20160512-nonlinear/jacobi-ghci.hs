-- GHCi Code for Jacobi Solver (Demo)
:load "secroot.hs"
:load "jacobi-sys.hs"
let fns = fromList 2 1 [(\[x, y, z] -> sin x + y**2 + log z - 3) . toList, (\[x, y, z] -> 3*x + 2**y - z**3) . toList, (\[x, y, z] -> x**2 + y**2 + z**3 - 6) . toList]
jacobiroots fns (fromList 3 1 [0, 0, 0]) 0.0000001 (\f x0 e -> secroot f x0 (x0+e) e)
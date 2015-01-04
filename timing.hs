

import System.Environment
import Data.List



-- dispatcher ["-a"] = print $ dafunc [1..33333333]
-- dispatcher ["-b"] = print $ dafunc [1..66666666]
-- dispatcher ["-c"] = print $ dafunc [1..99999999]

dispatcher ["-a"] = print $ dafunc [1..333333]
dispatcher ["-b"] = print $ dafunc [1..666666]
dispatcher ["-c"] = print $ dafunc [1..999999]


main = getArgs >>= dispatcher


dafunc :: (Num a) => [a] -> [a]


dafunc xs = dafunc' xs []
    where dafunc' [] ys = ys
          dafunc' (x:xs) ys = dafunc' xs (x:ys)

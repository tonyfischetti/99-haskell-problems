



import System.Environment
import Data.List



dispatcher ["-a"] = print $ dafunc [1..33333333]
dispatcher ["-b"] = print $ dafunc [1..66666666]
dispatcher ["-c"] = print $ dafunc [1..99999999]

main = getArgs >>= dispatcher

dafunc xs = head (reverse xs)




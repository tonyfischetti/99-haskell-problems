

------------------------------------------------------
------- 1. Find the last element of a list. ----------
------------------------------------------------------
last' :: [xs] -> xs

last' xs = head (reverse xs)

-- with pattern matching
last'' []      = error "No end for empty lists"
last'' [x]     = x
last'' (_:xs)  = last'' xs

-- with guards
last''' (x:xs)
    | null xs   = x
    | otherwise = last''' xs

-- other solutions from website
myLast'' = foldr1 (flip const)

myLast''' = head . reverse

myLast'''' = foldl1 (curry snd)

myLast''''' [] = error "No end for empty lists!"  
myLast''''' x  = x !! (length x -1)
------------------------------------------------------
------------------------------------------------------


-----------------------------------------------------
---- 2. Find the last but one element of a list. ----
-----------------------------------------------------
almost_last :: [xs] -> xs

almost_last []        = error "empty list"
almost_last (x:[])    = error "just one item"
almost_last (x:xs:[]) = x
almost_last (_:xs)    = almost_last xs

almost_last' xs = head (reverse (take 2 (reverse xs)))

-- with low precedence right-associative function applicator
almost_last'' xs = head $ reverse $ take 2  $ reverse xs

almost_last''' (x:xs)
    | null xs        = error "just one item"
    | length xs == 1 = x
    | otherwise      = almost_last''' xs

-- point free style
almost_last'''' = head . reverse . take 2  . reverse

-- other solutions from website
myButLast = last . init
 
myButLast' x = reverse x !! 1
 
myButLast'' [x,_]  = x
myButLast'' (_:xs) = myButLast'' xs
 
myButLast''' (x:(_:[])) = x
myButLast''' (_:xs) = myButLast''' xs
 
myButLast'''' = head . tail . reverse
-----------------------------------------------------
-----------------------------------------------------


------------------------------------------------------------
----- 3. Find the K'th element of a list. (1 indexed) ------
------------------------------------------------------------
kthel :: [xs] -> Int -> xs

kthel [] _      = error "out of bounds"
kthel xs 1      = head xs
kthel (_:xs) n  = kthel xs (n-1)

kthel' xs n = xs !! n - 1
------------------------------------------------------------
------------------------------------------------------------




main = do
        let ls  = [1..5]
        let lsl = [1, 2, 3, 4, 5, 6, 5, 1, 2, 3]
        let wds = "haskell"
        print (last lsl)
        print (kthel lsl 8)
        print (kthel' lsl 8)



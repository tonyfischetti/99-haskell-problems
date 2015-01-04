

import Data.List
-- needed for strict eval foldl, foldl'

------------------------------------------------------
------- 1. Find the last element of a list. ----------
------------------------------------------------------
last' :: [xs] -> xs

-- .74s - 2.40s -  6.05s
last' xs = head (reverse xs)

-- with pattern matching
-- .01s - .02s - .02s
last' []      = error "No end for empty lists"
last' [x]     = x
last' (_:xs)  = last' xs

-- with guards
-- .01s - .02s - .02s
last' (x:xs)
    | null xs   = x
    | otherwise = last' xs

-- with case statements
-- .02s - .03s - .04s
last' xs = case xs of (x:[])   -> x
                      (x:xs)   -> last' xs


-- other solutions from website
-- .01s - .02s - .04s
last' = foldr1 (flip const)

-- .72s - 1.39s - 5.29s
last' = head . reverse

-- .01s - .02s - .02s
last' = foldl1 (curry snd)

-- .7s - 1.4s - 7.96s
last' [] = error "No end for empty lists!"  
last' x  = x !! (length x - 1)

-- REAL HASKELL SOLUTION IS TO USE last FUNCTION
--
-- performance notes:
    -- Both the pattern-matching and guard formulations
    -- using tail-call optimization are the most performant
    -- of my solutions
    --
    -- They are suprisingly more performant than !! indexing
    -- with the length function. I think this is because
    -- the length function is a killa, performance-wise
------------------------------------------------------
------------------------------------------------------


-----------------------------------------------------
---- 2. Find the last but one element of a list. ----
-----------------------------------------------------
almost_last :: [xs] -> xs

-- .01s - .02s - .03s
almost_last []        = error "empty list"
almost_last (x:[])    = error "just one item"
almost_last (x:xs:[]) = x
almost_last (_:xs)    = almost_last xs

-- .68s - 1.38s - 4.48s
almost_last xs = head (reverse (take 2 (reverse xs)))

-- with low precedence right-associative function applicator
-- .74s - 1.35s - 4.94s
almost_last xs = head $ reverse $ take 2  $ reverse xs

-- takes way too long
almost_last (x:xs)
    | null xs        = error "just one item"
    | length xs == 1 = x
    | otherwise      = almost_last xs

-- point free style
-- takes way too long
almost_last = head . reverse . take 2  . reverse

-- other solutions from website
-- .01s - .03s - .04
almost_last = last . init
 
-- .76s - 1.45s - 6.37s
almost_last x = reverse x !! 1
 
-- .01s - .02s - .02s
almost_last [x,_]  = x
almost_last (_:xs) = almost_last xs
 
-- .01s - .02s - .02s
almost_last (x:(_:[])) = x
almost_last (_:xs) = almost_last xs
 
-- .68s - 1.4s - 6.59s
almost_last = head . tail . reverse

-- THE REAL HASKELL SOLUTION IS TO USE last . init
--
-- performance notes:
    -- Again, the pattern-matching solutions with TCO
    -- is among the most performant
    --
    -- Also, 'last . init' is performant
    --
    -- The solution that uses guards sucks but that's
    -- probably because the 'length' function is called
    -- for every call to almost_last'''
-----------------------------------------------------
-----------------------------------------------------


------------------------------------------------------------
----- 3. Find the K'th element of a list. (1 indexed) ------
------------------------------------------------------------
kthel :: [xs] -> Int -> xs

-- .01s - .01s - .01s
kthel [] _      = error "out of bounds"
kthel xs 1      = head xs
kthel (_:xs) n  = kthel xs (n-1)

-- .01s - .01s - .01s
kthel xs n = xs !! (n - 1)

-- .02s - .03s - .04s
kthel xs n = last $ take n xs

-- .02s - .03s - .04s
-- MMmmmm, curry
kthel2 n = last . take n
kthel = flip kthel2

-- above equivalent to
kthel = flip (\n -> last . take n)

-- .01s - .02s - .03s
kthel2 n = head . drop (n - 1)
kthel = flip dafunc2

-- THE REAL HASKELL is to use the !! indexing operator
--
-- Performance notes:
    -- TCO pattern-matching is most performant
    -- but also as performant as '!!' indexing,
    -- in this case
    --
    -- head . drop is better that last . take
------------------------------------------------------------
------------------------------------------------------------


-------------------------------------------------------
------- 4. Find the number of elements of a list. -----
-------------------------------------------------------
length' :: [xs] -> Integer

-- using list comprehensions
-- .02s - .03s - .04s
length' xs = sum [1 | _ <- xs]

-- using recursion
-- CAUSES STACK OVERFLOW
length' [] = 0
length' xs = 1 + length' (tail xs)

-- using explicit if else
-- CAUSES STACK OVERFLOW
length' xs = if null xs
             then 0
             else 1 + length' (tail xs)

-- using guards
-- CAUSES STACK OVERFLOW
length' xs
    | null xs = 0
    | otherwise = 1 + length' (tail xs)

-- using pattern matching
-- CAUSES STACK OVERFLOW
length' (x:[]) = 1
length' (x:xs) = 1 + length' xs

-- .03s - .04s - .06s
length' = fst . last . zip [1..]

-- from the internet
-- uses tail recursion
-- .01s - .02s - .03s
length' xs = len xs 0
    where len [] l = l
          len (x:xs) l = len xs (l+1)

-- from the internet
-- change all values to 1 and sum it
-- .03s - .05s - .06s
length' = sum . map (\_ -> 1)

-- .02s - .03s - .03s
length' xs = foldl step 0 xs where step acc x = acc + 1

-- equivalent to above but using lambda
-- .01s - .03s - .05sj
length' = foldl (\acc _ -> 1 + acc) 0

-- CAUSES STACK OVERFLOW
length' =  foldr (\_ acc -> acc + 1) 0

-- strict version
-- .01s - .03s - .04s
length' = foldl' (\acc _ -> 1 + acc) 0

-- the real solution is to use the length function
--
-- performance notes:
    -- Any explicitly recursive function that cannot be
    -- TCO-ed causes a stack overflow
    --
    -- The explicit one that uses TCO is very performant,
    -- however
    --
    -- Similarly, the tail-recursive foldl works but the
    -- non-tail-recursive foldr does not work
-------------------------------------------------------
-------------------------------------------------------



-------------------------------------------------------
----------------- 5. Reverse a list ------------------
-------------------------------------------------------

reverse' :: (Num a) => [a] -> [a]

-- takes too long
reverse' [] = []
reverse' (x:[]) = [x]
reverse' xs = last xs : (reverse' (init xs))

-- .00s - .01s - .01s
reverse' = foldl' (\acc x -> x : acc) []

-- .00s - .01s - .01s
reverse' = foldl (flip (:)) []

-- takes too long
reverse' xs = foldr (\x acc -> acc ++ [x]) [] xs

-- .01s - .02s - .04s
reverse' xs = reverse'' xs []
    where reverse'' [] ys = ys
          reverse'' (x:xs) ys = reverse'' xs (x:ys)

-- the real solution is to use the reverse function
--
-- performance notes:
    -- The foldl (which I think are tail-recursive)
    -- are really the only function that will work
    -- with large lists. It is about as performant
    -- as 'reverse'
    -- The manual TCO version is very performant as well
-------------------------------------------------------
-------------------------------------------------------


main = do
        let ls  = [1..5]
        let lsl = [1, 2, 3, 4, 5, 6, 5, 1, 2, 3]
        let wds = "haskell"
        print (last lsl)
        print (kthel lsl 8)
        print (kthel' lsl 8)



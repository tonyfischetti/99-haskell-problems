

------------------ length --------------------
-- function definition
length' :: [xs] -> Integer

-- using list comprehensions
length' xs = sum [1 | _ <- xs]

-- using recursion
length'' [] = 0
length'' xs = 1 + length'' (tail xs)

-- using explicit if else
length''' xs = if null xs
               then 0
               else 1 + length''' (tail xs)

-- using guards
length'''' xs
    | null xs = 0
    | otherwise = 1 + length'''' (tail xs)

-- using pattern matching
-- length''''' (x:[]) = 1
length''''' (x:[]) = 1
length''''' (x:xs) =  1 + length''''' xs
----------------------------------------------




------------------ sum --------------------
sum' :: (Num xs) => [xs] -> xs

sum' xs = foldr (+) 0 xs

sum'' xs = foldr (\starting x -> starting + x) 0 xs

sum''' [] = 0
sum''' numbs = (head numbs) + sum'' (tail numbs)
-------------------------------------------


--
------ maximum --------------
maximum' :: (Ord x) => [x] -> x
maximum' (x:[]) = x
maximum' (x:y:[])
    | x > y  = x
    | x <= y = y
maximum' (x:y:zs) = maximum' ((maximum' (x:[y])):zs)
-- (same thing)
-- maximum' (x:y:zs) = maximum' $ (maximum' $ x:[y]) : zs
--
-- other solutions from internet
maximum'' :: (Ord a) => [a] -> a  
maximum'' [] = error "maximum of empty list"  
maximum'' [x] = x  
maximum'' (x:xs) = max x (maximum' xs)  
-----------------------------
--
--
--

------- replicate ------------
replicate' 1 y = [y]
replicate' x y = y : replicate' (x-1) y

replicate'' x y
    | x < 2 = [y]
    | otherwise = y : replicate'' (x-1) y


-- other answers from the internet
replicate''' :: (Num i, Ord i) => i -> a -> [a]  
replicate''' n x  
    | n <= 0    = []  
    | otherwise = x:replicate''' (n-1) x  
------------------------------



------ take ---------
take' :: Int -> [xs] -> [xs]
take' 1 (x:_) = [x]
take' n (x:[]) = [x]
take' n (x:xs) = x : take' (n-1) xs
---------------------

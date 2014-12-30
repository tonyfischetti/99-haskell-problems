




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

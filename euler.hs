-- Problem 1
p1 = sum $ filter (\x -> rem x 3 == 0 || rem x 5 == 0) [1..999]

-- Problem 2
fib a b = [a,b] ++ fib' b (a + b) where
    fib' a b = [b] ++ fib' b (a + b)

p2 = sum $ filter even $ takeWhile (<=4000000) $ fib 1 2

-- Problem 3
p3 n = p3' n 2 where
    p3' n x | n == x = x 
            | n `mod` x == 0 = p3' (div n x) x
            | otherwise = p3' n (succ x)  

-- Problem 4
-- 10001 - 997799
palindromes = reverse $ make1 ++ make2 where
    -- 10001-99999
    make1 = concat $ map (\y -> map (\x -> x ++ y ++ reverse x) $ map show [10..99]) $ map show [0..9]     
    -- 100001-997799 
    make2 = concat $ map (\y -> map (\x -> x ++ y ++ y ++ reverse x) $ map show [10..99]) $ map show [0..7]
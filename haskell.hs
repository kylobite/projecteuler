-- Problem 1
p1 = sum $ filter (\x -> rem 3 == 0 || rem x 5 == 0) [1..999]

-- Problem 2
fib a b = [a,b] ++ fib' b (a + b) where
    fib' a b = [b] ++ fib' b (a + b)

p2 = sum $ filter even $ takeWhile (<=4000000) $ fib 1 2

-- Problem 3
p3 n = p3' n 2 where
    p3' n x | n == x = x 
            | n `mod` x == 0 = p3' (n `div` x) x
            | otherwise = p3' n (succ x)  
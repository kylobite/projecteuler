-- Problem 1
p1 = sum $ filter (\x -> rem x 3 == 0 || rem x 5 == 0) [1..999]

-- Problem 2
fib a b = [a,b] ++ fib' b (a + b) where
    fib' a b = [b] ++ fib' b (a + b)

p2 = sum $ filter even $ takeWhile (<=4000000) $ fib 1 2

-- Problem 3
p3 n = p3' n 2 where
    p3' n x | n == x = x 
            | n `rem` x == 0 = p3' (div n x) x
            | otherwise = p3' n (succ x)  

-- Problem 4
-- Generate palindromes
palindromes = reverse $ (make 1 9) ++ (make 2 7) where
    make a b = concat $ map (\y -> map (\x -> y ++ x ++ reverse y) $ map (concat.replicate a) $ map show [0..b]) $ map show [10..99]

p4 = f (map (\x -> read x :: Int) palindromes) (reverse [100..999]) where
        f x y = if length (filter (\z -> rem (head x) z == 0 && (length $ show $ div (head x) z) == 3) y) > 0
                then head x
                else f (tail x) y

-- Problem 5
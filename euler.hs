-- Problem 1
p1 = sum $ filter (\x -> rem x 3 == 0 || rem x 5 == 0) [1..999]

-- Problem 2
fib a b = [a,b] ++ fib' b (a + b) where
    fib' a b = [b] ++ fib' b (a + b)

p2 = sum $ filter even $ takeWhile (<=4000000) $ fib 1 2

-- Problem 3
p3 n = p3' n 2 [] where
    p3' n x c | n == x = (x:c)
              | n `rem` x == 0 = p3' (div n x) x (x:c)
              | otherwise = p3' n (succ x) c

-- Problem 4
palindromes = reverse $ (pd 1 9) ++ (pd 2 7)

pd a b = concat $ map (\y -> map (\x -> y ++ x ++ reverse y) $ map (concat.replicate a) $ map show [0..b]) $ map show [10..99]

p4 = f (map (\x -> read x :: Int) palindromes) (reverse [100..999]) where
        f x y = if length (filter (\z -> rem (head x) z == 0 && (length $ show $ div (head x) z) == 3) y) > 0
                then head x
                else f (tail x) y

-- Problem 5
p5 n = foldl1 lcm [2..n]

-- Problem 6
p6 n = (sum [1..n] * sum [1..n]) - sum [x * x | x <- [1..n]]

-- Problem 7
primes = [2] ++ filter test [3,5..] where
	test x = not $ or $ map (\y -> rem x y == 0) [2..x - 1]

p7 n = last $ take n primes

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
palindromes = reverse $ make1 ++ make2 where
    -- 10001-99999
    make1 = concat $ map (\y -> map (\x -> y ++ x ++ reverse y) $ map show [0..9]) $ map show [10..99]     
    -- 100001-997799 
    make2 = concat $ map (\y -> map (\x -> y ++ x ++ x ++ reverse y) $ map show [0..7]) $ map show [10..99]

-- Check if number is three digits and the quotient is three digits
using3 a b = let m = rem a b
                 n = div a b
             in if m == 0 && (length $ show n) == 3
                then True
                else False

p4  = let p = map (\x -> read x :: Int) palindromes
          d = reverse [100..999]
      in f p d where
        f :: [Int] -> [Int] -> Int
        f [] _     = 0
        f (x:xs) y = let n = filter (\z -> using3 x z) y
                     in if length n > 0
                        then x
                        else f xs y

-- Problem 5
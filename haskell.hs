-- Problem 1
p1 = sum $ filter (\x -> rem 3 == 0 || rem x 5 == 0) [1..999]

-- Problem 2
fib a b = [a,b] ++ fib' b (a + b) where
    fib' a b = [b] ++ fib' b (a + b)

p2 = sum $ filter even $ takeWhile (<=4000000) $ fib 1 2

-- Problem 3
-- Please forgive me for the code that follows
primes = [2] ++ filter (\x -> not $ or $ map (\y -> rem x y == 0) [2..x - 1]) [3,5..]

p3 x = let plist = takeWhile (<=(div x 2)) primes
       in last $ check x plist [] where
        check :: Int -> [Int] -> [Int] -> [Int]
        check x [] c     = c
        check x (p:ps) c = if rem x p == 0
                            then let r = reduce x p []
                                     y = fst r
                                     d = snd r
                                 in if y > 1
                                        then check y ps (c ++ d)
                                        else (c ++ d)
                            else check x ps c
        reduce a b c = if rem a b == 0
                        then if a `div` b > 1
                                then reduce (div a b) b (b:c)
                                else ((div a b),(a:c))
                        else (a,c)
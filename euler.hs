import Data.Char (digitToInt)

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
primes' = [2] ++ filter test [3,5..] where
    test x = not $ or $ map (\y -> rem x y == 0) [2..x - 1]

p7 n = last $ take n primes'

-- Problem 8
p8 = let g = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858" ++
             "6156078911294949545950173795833195285320880551112540698747158523863050715693290963295227443043557668966" ++ 
             "4895044524452316173185640309871112172238311362229893423380308135336276614282806444486645238749303589072" ++ 
             "9629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010" ++
             "5336788122023542180975125454059475224352584907711670556013604839586446706324415722155397536978179778461" ++
             "7406495514929086256932197846862248283972241375657056057490261407972968652414535100474821663704844031998" ++
             "9000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294" ++ 
             "7654568284891288314260769004224219022671055626321111109370544217506941658960408071984038509624554443629" ++
             "8123098787992724428490918884580156166097919133875499200524063689912560717606058861164671094050775410022" ++
             "5698315520005593572972571636269561882670428252483600823257530420752963450"
     in maximum $ get' g [] where
        get' :: String -> [Int] -> [Int]
        get' (_:_:_:_:[]) c = reverse c
        get' (x:xs) c       = get' xs (((digitToInt x) * (product $ (map (read . (:"")) (take 4 xs) :: [Int]))):c)

-- Problem 9
p9 = product $ 
     map round $ 
     sqrtList $ head $ 
     filter (\x -> (sum $ sqrtList x) == 1000) $
     triplets [(x*x,y*y,x*x + y*y)|x<-[1..990],y<-[1..990]] where
        triplets xs = filter check xs where
            check (a,b,c) = isSquare c
            isSquare x = let y = round . sqrt $ (fromIntegral x :: Double)
                         in  x == y*y
        sqrtList (a,b,c) = let a' = sqrt $ (fromIntegral a :: Double)
                               b' = sqrt $ (fromIntegral b :: Double)
                               c' = sqrt $ (fromIntegral c :: Double)
                           in [a',b',c']

-- Problem 10
primes = [2] ++ filter isPrime [3,5..] where
    isPrime a = isPrime' a primes
    isPrime' a (p:ps)
        | p*p > a      = True
        | mod a p == 0 = False
        | otherwise    = isPrime' a ps   

p10 = sum $ takeWhile (<=2000000) primes






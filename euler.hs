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
sort4max = maximum $ get' g [] where
    get' :: String -> [Int] -> [Int]
    get' (_:_:_:_:[]) c = reverse c
    get' (x:xs) c       = get' xs (((digitToInt x) * (product $ (map (read . (:"")) (take 4 xs) :: [Int]))):c)

p8 = sort4max "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858" ++
              "6156078911294949545950173795833195285320880551112540698747158523863050715693290963295227443043557668966" ++ 
              "4895044524452316173185640309871112172238311362229893423380308135336276614282806444486645238749303589072" ++ 
              "9629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010" ++
              "5336788122023542180975125454059475224352584907711670556013604839586446706324415722155397536978179778461" ++
              "7406495514929086256932197846862248283972241375657056057490261407972968652414535100474821663704844031998" ++
              "9000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294" ++ 
              "7654568284891288314260769004224219022671055626321111109370544217506941658960408071984038509624554443629" ++
              "8123098787992724428490918884580156166097919133875499200524063689912560717606058861164671094050775410022" ++
              "5698315520005593572972571636269561882670428252483600823257530420752963450"

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

-- Problem 11
q = "08 02 22 97 38 15 00 40 00 75 04 05 07 78 52 12 50 77 91 08\n"
 ++ "49 49 99 40 17 81 18 57 60 87 17 40 98 43 69 48 04 56 62 00\n"
 ++ "81 49 31 73 55 79 14 29 93 71 40 67 53 88 30 03 49 13 36 65\n"
 ++ "52 70 95 23 04 60 11 42 69 24 68 56 01 32 56 71 37 02 36 91\n"
 ++ "22 31 16 71 51 67 63 89 41 92 36 54 22 40 40 28 66 33 13 80\n"
 ++ "24 47 32 60 99 03 45 02 44 75 33 53 78 36 84 20 35 17 12 50\n"
 ++ "32 98 81 28 64 23 67 10 26 38 40 67 59 54 70 66 18 38 64 70\n"
 ++ "67 26 20 68 02 62 12 20 95 63 94 39 63 08 40 91 66 49 94 21\n"
 ++ "24 55 58 05 66 73 99 26 97 17 78 78 96 83 14 88 34 89 63 72\n"
 ++ "21 36 23 09 75 00 76 44 20 45 35 14 00 61 33 97 34 31 33 95\n"
 ++ "78 17 53 28 22 75 31 67 15 94 03 80 04 62 16 14 09 53 56 92\n"
 ++ "16 39 05 42 96 35 31 47 55 58 88 24 00 17 54 24 36 29 85 57\n"
 ++ "86 56 00 48 35 71 89 07 05 44 44 37 44 60 21 58 51 54 17 58\n"
 ++ "19 80 81 68 05 94 47 69 28 73 92 13 86 52 17 77 04 89 55 40\n"
 ++ "04 52 08 83 97 35 99 16 07 97 57 32 16 26 26 79 33 27 98 66\n"
 ++ "88 36 68 87 57 62 20 72 03 46 33 67 46 55 12 32 63 93 53 69\n"
 ++ "04 42 16 73 38 25 39 11 24 94 72 18 08 46 29 32 40 62 76 36\n"
 ++ "20 69 36 41 72 30 23 88 34 62 99 69 82 67 59 85 74 04 36 16\n"
 ++ "20 73 35 29 78 31 90 01 74 31 49 71 48 86 81 16 23 57 05 54\n"
 ++ "01 70 54 71 83 51 54 69 16 92 33 48 61 43 52 01 89 19 67 48\n"

q' = "00 01 02\n03 04 05\n06 07 08\n09 10 11"

split = map words $ lines q'

horizontal = (map . map) (\x -> read x :: Int) split
-- [[0,1,2],[3,4,5],[6,7,8],[9,10,11]]

vertical = shift 0 horizontal [] where
    shift i xs c = if i == (length xs - 1)
                   then c
                   else shift (succ i) xs ((map (!! i) xs):c)

diagonal = loop horizontal [] where
    loop [] c = c
    loop xs c = loop (tail xs) ((map (\x -> shift x xs) [0..(length xs)-1]) : c)
    --shift :: Int -> [[Int]] -> [Int] -> [Int]
    --shift i xs c = if i == (length xs - 1)
    --               then c
    --               else shift (succ i) xs ((shift' i xs):c)
    shift i xs = let a = drop i (xs !! i)
                  in if a == []
                     then 1
                     else head a) (tail xs) ((map (!! i) xs):c)
















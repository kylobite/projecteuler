-- Problem 1
p1 = sum $ filter (\x->rem 3 == 0 || rem x 5 == 0) [1..999]
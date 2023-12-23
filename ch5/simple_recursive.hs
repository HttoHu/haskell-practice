factorial::Integer->Integer 
factorial n = if n < 0 then error "n is less than 0"
              else if n==1 then 1
              else n * factorial(n-1)

power::Int->Int->Int
power 0 0 = 1
power _ 0 = 1
power x n | odd n = let p = power x ((n-1) `div` 2) in x*p*p
            | otherwise = let p = power x (n `div` 2) in p*p 
prod:: [Int]->Int 
prod [] = 1
prod (x:xs) = x * prod xs


snoc::a->[a]->[a]
snoc x [] = [x]
snoc x (y:ys)  = y : (snoc x ys)

last'::[b]->b 
last' [] = error "call last on empty list"
last' [x] = x 
last' (x:xs) = last' xs

take'::Int->[a]->[a] 
take' n _ | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take (n-1) xs

elem'::[a]->Int->a 
elem' [] _ = error "elem' overflow"
elem' (x:xs) n = if n==0 then x else elem' xs (n-1)

del::[Int]->Int->[Int] 
del [] _ = error "delete element on empty list"
del (x:xs) y = if x==y then xs else x : del xs y

mc::Int->Int
mc n | n > 100 = n -10 
     | n <= 100 = mc (mc (n+11))

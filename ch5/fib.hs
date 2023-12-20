import Data.ByteString (cons)
-- fib::(Eq a, Num a)=> a->a 
-- fib 0 = 0
-- fib 1 = 1 
-- fib x = fib(x-1) +fib(x-2)

fibStep::(Num a)=>(a,a)->(a,a)
fibStep (a,b)=(b,a+b)

fibPair::(Eq a,Num a)=>a->(a,a)
fibPair 0 = (0,1)
fibPair n = fibStep(fibPair (n-1))

fibs' :: Num b => Int -> [b]
fibs' n = take n (map fst (iterate fibStep(0,1)))

fib' :: (Eq t1, Num t1, Num t2) => t1 -> t2 -> t2 -> t2
fib' 0 f1 f2 = f1
fib' n f1 f2 = fib' (n-1) f2 (f1+f2)

fib :: (Eq b, Num b) => b -> b
fib n = fib' n 0 1

fibPairs :: (Eq a, Num a, Enum a) => a -> [(a, a)]
fibPairs n = map fibPair [1..n]

combine :: Num b => [(b, b)] -> [(b, b)]
combine ((a,b):(c,d):rest) = (a*d,b*b) : combine((c,d):rest)
combine _ = []



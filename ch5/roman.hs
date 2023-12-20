romeNotation::[String]
romeNotation= ["M","CM","D","CD","C","XC","L","XL","X","IX","V","IV","I"]

romeAmount::[Int]
romeAmount=[1000,900, 500, 400, 100, 90, 50, 40, 10, 9, 5, 4, 1]

pair :: [(String, Int)]
pair = zip romeNotation romeAmount

subtrahead :: Int -> (String, Int)
subtrahead n = head(dropWhile (\(_,a)-> a > n ) pair)

convert :: Int -> String
convert 0 = ""
convert n = let (lit,val) = subtrahead n
            in lit ++ convert (n-val)
-- convert rome to dec


-- convert deci to binary
ctb ::Int -> String
ctb 0 = "0"
ctb 1 = "1"

ctb n = ctb(n `div` 2) ++ ctb(n `mod` 2)
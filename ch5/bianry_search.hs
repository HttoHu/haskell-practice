search::(Ord a) => a->[a]->Bool
search a [] = False 
search a xs | a > m = search a right 
            | a < m = search a left 
            | otherwise = True 
            where(left,m:right) = splitAt (length xs `div` 2) xs

-- 练习5.8.1 
search'::(Ord a)=>a->[a]->[a]
search' x [] = []
search' x xs | x > m = search' x right
             | x < m = search' x left
             | otherwise = search' x left ++ [x] ++ search' x right
             where(left,m:right) = splitAt(length xs `div` 2) xs
insert :: (Ord t) => t -> [t] -> [t]
insert x [] = [x]
insert x (a : xs)
  | x < a = x : a : xs
  | otherwise = a : insert x xs

insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x : xs) = insert x (insertionSort xs)

swaps :: (Ord t) => [t] -> [t]
swaps [] = []
swaps [x] = [x]
swaps (x1 : x2 : xs)
  | x1 > x2 = x2 : swaps (x1 : xs)
  | otherwise = x1 : swaps (x2 : xs)

fix :: Eq a => (a -> a) -> a -> a
fix f x = if x == x' then x else fix f x' 
            where x' = f x 

bubbleSort :: Ord t => [t] -> [t]
bubbleSort  = fix swaps  

bubbleSort2 xs = bubbleSort excludeLast ++ [lastEle]
            where swapRes = swaps xs 
                  excludeLast = init swapRes
                  lastEle = last swapRes 
quickSort::(Ord a)=>[a]->[a]
quickSort [] = []
quickSort [x] =[x]
quickSort (x:xs) = quickSort(left) ++ x:quickSort(right)
               where (left,right) = filterSplit (<= x) xs 

filterSplit::(a->Bool) -> [a]->([a],[a])
filterSplit _ [] = ([],[])
filterSplit f (x:xs) = if (f x) then (x:l,r) else (l,x:r)
                    where (l,r) = filterSplit f xs 
  
merge::(Ord a)=>[a]->[a]->[a]
merge xs [] = xs
merge [] ys = ys
merge (a:l) (b:r) = if a < b then a:merge l (b:r)
                      else b:merge (a:l) r
                      
msort::(Ord a)=>[a]->[a]
msort [] = []
msort [x] = [x]
msort xs = merge (msort l) (msort r)
          where l = take half xs  
                r = drop half xs 
                half = (length xs) `div` 2
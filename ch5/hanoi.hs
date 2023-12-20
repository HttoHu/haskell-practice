move::(Int,Int,Int,Int)->[(Int,Int)]
move(0,_,_,_)=[]
move(i,from,to,via) = move(i-1,from,via,to) 
                     ++[(from,to)]++move(i-1,via,to,from)

hanoi :: Int -> [(Int, Int)]
hanoi n = move(n,1,2,3)
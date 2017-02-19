map' ::  (a -> b)  -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x : map' f xs
---folds --- 
--foldl' f acc [x] =  acc f x
--foldl' f acc (x:xs) = foldl' f (acc f x ) xs
---------
--
--foldr' f acc [x] = x f acc
--foldr' f acc (x:xs) = x f  foldr' f acc xs
----searching for an item in listi
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let small= [y | y <- (x:xs) ,y < x] 
        equal = [y |y <- (x:xs) , y==x]
	grtr = [y | y <- (x:xs) , y > x ]
     in qsort small ++ equal  ++  qsort grtr
search' a [] = False
search' a (xs) = if (length ( [ y | y <-xs, y==a] ) > 0) then True else False
             

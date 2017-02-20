map' ::  (a -> b)  -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x : map' f xs
---folds --- 
--foldl' f acc [] = acc
foldl' f acc (xs) = if ( length xs == 1 ) then acc f head xs else foldl' f ( acc f head xs) drop 1 xs
---------
--
--foldr' f acc [x] = x f acc
--foldr' f acc (x:xs) = x f  foldr' f acc xs
----searching for an item in list
--Quick Sort function
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) =
    let small= [y | y <- (x:xs) ,y < x] 
        equal = [y |y <- (x:xs) , y==x]
	grtr = [y | y <- (x:xs) , y > x ]
     in qsort small ++ equal  ++  qsort grtr
--- search' a xs : a function that returns a boolean value if 'a' is found in the list xs then returns True otherwise returns False
search' a [] = False
search' a (xs) = if (length ( [ y | y <-xs, y==a] ) > 0) then True else False
-- getIndex a xs return an integer value : if 'a' is found in list xs then returns its index in xs otherwise returns -1
getIndex a (xs) = if search' a xs then length ( [ y | y <- xs , y< a]) else -1           

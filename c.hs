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

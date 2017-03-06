myTake :: [a] -> Int -> [a]
myTake [] n =[]
myTake (x:xs) n 
         |n <= 0 = []
         | otherwise  = x:myTake xs (n-1)
myDrop :: [a]->Int -> [a]
myDrop [] n = []
myDrop (x:xs) n 
        |n >= length (x:xs)  = []
        |n <= 0  = (x:xs)
        |otherwise  = myDrop xs (n-1)
myZip :: [a] -> [b] ->[(a,b)]
myZip [] _ =[]
myZip _ [] =[]
myZip (x:xs) (y:ys) = (x,y): myZip xs ys  
myConcat :: [[a]] -> [a]
myConcat []= []
myConcat xss = [x|xs <- xss, x<-xs]              
myElem :: (Eq a ) =>a-> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) 
          | y == x  = True
          | otherwise = myElem x ys
myMerge ::(Ord a) =>  [a] ->[a] -> [a]
myMerge  (xs)  [] = xs
myMerge [] xs  = xs
myMerge (x:xs) ( y: ys)
               | x > y = y: myMerge (x:xs) ys
               | otherwise = x:myMerge xs (y:ys)
--Higher Order Function
twice :: (a->a) ->  a -> a
twice f x  = f $f x
myMap :: (a -> b ) -> [a] -> [b]
myMap f  (x:xs)  = f x  : myMap f  xs
myFilter  :: (a -> Bool) -> [a]  ->[a]
myFilter f (x:xs) | f x = x:myFilter f xs
                  | otherwise  = myFilter f xs
--folds
myFoldr :: ( a -> b  -> b ) ->b -> [a] ->b
myFoldr f acc [] = acc
myFoldr f acc (x:xs) = f x $ myFoldr f acc xs
--Foldl
myFoldl :: ( a-> b -> b) -> b -> [a] -> b
myFoldl f acc [x] = f x acc
myFoldl f acc (x: xs)  = myFoldl f ( f x acc) xs

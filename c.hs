map' ::  (a -> b)  -> [a] -> [b]
map' _ [] = [] 
map' f (x:xs) = f x : map' f xs
---folds --- 
foldl' f acc (x:xs) =  
                 let a = length (x:xs)
              if ( a > 0 ) 
               then
                     let z = 1 
                    let q = acc f x
                    until (z < a) 
                          { q = q f head xs 
                             xs = drop 1 xs
                          }        
                     in q 
               else  
                   in acc 

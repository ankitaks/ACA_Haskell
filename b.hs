--starting once again.......
-- b. hs
merge :: (Ord a ) => [a] -> ( [a] -> [a] )
merge [] xs    = xs
merge xs [] = xs
merge (x:xs) (y:ys) = if  x < y then [x] ++ merge xs (y:ys) else [y] ++ merge (x:xs) ys
---Taylor Series
fact x = if x ==1 then 1 else x * fact x-1
pow (x, n) = if n == 0 then 1 else x* pow (x, n-1)
--pn x n = [ y | y <- x , y =( (pow -1 k)* pow x (2*k + 1) )/fact (2*k +1 ) | k <- [0..n] ]
tn ( x,  n) = ((-1)**n) * (x** (2*n+1))  / fact (2*n+1)
--tn' :: (Floating t, Num t1) => ( t,t1) -> t
--tn' (x,n) = (pow -1 n)*(pow x (2*n+1)) / fact (2*n +1 )
--sum' :: (Floating a ,Int n) => (a,n) ->a
sum' (x ,n ) =
             let  b = [tn (x,y ) | y <- [0..n] ]
             in sum b
-- pn x n  = Pn(x) in Taylor Series
pn x n  = sum' (x,n)
en x n  = sin x - pn x n


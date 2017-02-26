--helper functions
import Data.List
won xs n c=
  let y =  group xs
      y' = group (transP xs n)
      diag = group (dia xs n ++ "*")
      z = replicate n c
      v = elem z y
      v' =  elem z y'
      vs = elem z diag
  in (v||v'||vs)
dia xs n  = [xs !! x | x <- [0,(n+1)..((n*n)-1)]]
transP xs n =[ xs !! y | x<-[0..(n-1)],y <- [x,x+n..(n*(n-1)+x)]]
isOver :: [Char] -> Int ->Char-> String
isOver xs n c
        | won xs n c = "Player"++ getNum c ++"Wins !!"
        | elem '_' xs = "NO"
        | otherwise  = "It's a Tie !! \n Game Over!!"
getNum c
        | c == 'X' =['1']
        | otherwise = ['2']
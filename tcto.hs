import Data.Array.IO
import Data.List
import System.IO
import Data.Typeable
--import htcto.hs
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
----end of htcto.hs
---reads a integer
prompt s =
    do
      putStr s
      line <- getLine
      return (read line)
getInt :: (Read b, Num b) =>String -> IO b
getInt = prompt
----
getNum c
        | c == 'X' =['1']
        | otherwise = ['2']
addSpace :: [Char] -> [Char]
addSpace [] = []
addSpace (x:xs) = ' ':x:' ': addSpace xs
display :: [Char] -> Int -> IO()
display [] n = putStrLn " "
display xs n =
 do
      let y =  (take n xs)
      putStrLn (addSpace y)
      display  (drop n xs)  n
myMat ::(Num a, Ord a)=>a-> [Char]
myMat n
       | n < 1 = []
       | otherwise = '_':myMat (n-1)
convert :: Int -> Int ->Int -> Int
convert n i j  = n*(i-1) + j
--isValidNum n  =  typeOf n == typeOf 1
isValid :: Int -> Int -> Int -> Bool
isValid n i j
       | i <0 || j < 0 || i > n || j > n  = False
       |otherwise = True
isEmpty :: [Char] ->Int -> Int -> Int -> Bool
--assuming i j are valid
isEmpty xs n i j = xs !! ((convert n i j )-1)== '_'
update :: [Char] -> Char -> Int -> [Char]
update xs c n = take (n-1) xs ++ [c] ++ drop (n) xs
myflip c
     |c == 'X' = 'O'
     | otherwise = 'X'

startGame xs n c=
              do
                 putStrLn ( "Player "++ (getNum c) ++" turn !! ")
                 i <- getInt "Enter x- co-ordinate : "
                 j <- getInt  "Enter y- co-ordinate : "
                 if (isValid n i j)
                  then
                      do
                         if isEmpty xs n i j
                         then
                           do
                             let xn = update xs c (convert n i j)
                             let y = isOver xn  n c
                             display xn n
                             if (y == "NO" ) then startGame xn n (myflip c)
                               else putStrLn y
                         else
                            do
                               putStrLn "OOPS !! given position is Non-Empty Try Again!!"
                               startGame xs n c
                  else
                    do
                        putStrLn "OOPS !! given position is INVALID :-( Try Again!!"
                        startGame xs n c


main =
       do
          n <- getInt "Enter the size  of board : "
          let  xs = myMat (n*n)
          display xs n
          startGame xs n 'X'

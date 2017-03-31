-- profit : a function that takes the list and maps it to a real number.
-- convert the list to a matrix and calculate the profit.case
import Data.List
import Data.Function (on)
import qualified Data.Map as Map
-- ================================================================================
data Board=Board
     {
      empty::[Int]
      , listX ::[Int]
      ,listO :: [Int]
      ,profit :: Int
      ,gameStatus:: Int
      ,gameSize :: Int
     } deriving (Show)
                          ------Board Operations
-- ================================================================================
main =
 do
   n <- getInt " Enter Size of the Board  "
   let k =  Board{profit=0,gameSize = n, gameStatus= 2, listX =[],listO =[],empty = [1..(n*n)]}
   playGame k False
-- ================================================================================
first (a,_) = a
secnd (a,b) = b
checkSize n =  n>2 && n<= 10
-- ================================================================================
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
-- ================================================================================
--playGame :: Board -> Bool ->Board  game = Board{empty=[1..gs],listX=[],listO=[],profit=0,gameStatus=2,gameSize = gs}
playGame b1 cpuTurn  =
 do
          let n =  gameSize b1
   -- if(validate pos n)
 --       then
          if(cpuTurn)
              then
              do
                putStrLn "CPUs turn"
                let b2 = first (maximum' ((map(\x -> (x,minimax x 0 False))[updateX b1 y |y <- empty b1] ) ) )
                    gs = gameStatus b2
                display (genList b2) (n)
                if(gs == 1)then do putStrLn " CPU Wins !!"
                else
                    if(gs == 0 ) then do putStrLn "Its Tie"
                     else
                      do
                         playGame b2 False
          else
                do
                  putStrLn "Your Turn"
                  pos<- getInt "Enter position of O "
                  let  b2 = updateO b1 pos
                       gs = gameStatus b2
                  display (genList b2) (n)
                  if ( gs == -1 )
                   then do putStrLn "You Won!!"
                  else
                   if (gs ==  0)
                   then do putStrLn "Tie!!"
                    else do playGame b2 True
  --  else
    --   do
--        putStrLn "OOPS !! given position is INVALID :-( Try Again!!"
   --     n1 <- getInt "Enter the position for O "
      --  playGame b1 False n1


-- ==

--   else
  --  do
    --   putStrLn "Improper Size"
-- ================================================================================
validate :: Int -> Int -> Bool
validate i n = ( i >=1 && i<=(n^2) )
-- ================================================================================
minimax :: Board ->Int->Bool-> Int
minimax brd1 depth maximize =
    if ( gameStatus brd1 /=2  || depth == 4) then (profit brd1)
    else
       if (maximize)
       then   max  (profit brd1) (minimax  (first (findX brd1))  (depth+1) False)
       else   min  (profit brd1) (minimax ( secnd (findX brd1)) (depth+1)  True)
-- ==
maxt :: (Board,Int)->(Board,Int)->(Board,Int)
maxt a b
     |secnd a > secnd b  = a
     |otherwise =  b
minimum' :: (Board,Int)->(Board,Int)->(Board,Int)
minimum' a b
    |secnd a > secnd b = b
    |otherwise = a
-- ================================================================================
findX :: Board -> (Board,Board)
findX x =
               let

                  e = empty x
                  xb = [updateX x y|y<-e]
                  ob = [updateO x y | y<-e]
                  xB =first ( maximum' ( (map(\z -> (z, profit z) ) xb)))
                  xO = first ( maximum' ((map(\z -> (z, (-1)*profit z) ) ob)) )
                in (xB,xO)   --  xB =  Max X; xO = Max O
-- ================================================================================
data Profit = Profit
        {
          pX :: [[Char]]
          ,pO :: [[Char]]
          ,gm :: [[Char]]
          ,ne :: Int
          ,gl :: Int
        } deriving (Show)
-- ================================================================================

findP :: Board -> Profit
findP x =
        let   y =  createBoard x
              n = gameSize x
              nE = length $ empty x
              y' = transpose y
              d1 =group( [(y!!i)!! i | i<-[0..(n-1)]])
              d2 =group ( [(y!!i)!!(n-1-i)|i<-[0..(n-1)]])
              d =  [d1++d2]
              plt = (map (\x -> group x) y ) ++ (map (\x -> group x) y') ++ (d)
              pl = concat plt
              pl_X = filter ( \x -> (x!!0 == 'X') ) pl
              pl_O = filter ( \x -> (x!!0 == 'O') ) pl
              k = Profit{pX = pl_X,pO = pl_O,gm = pl,ne=nE,gl=n}
              in k
-- ================================================================================
findPr :: Profit->Int
findPr k=
         let
            x = pX k
            o = pO k
            vX=foldl (\acc x -> acc + x) 0  (map (\q-> (10^(length q))) x )
            vO= foldl (\acc x -> acc + x) 0 (map (\q-> (10^(length q))) o)
            gs= findGS (pX k) (pO k) (gl k)-- +1 = X wins  ; 0 :  Game On ; -1 : O wins ; 2 : game Tie
          in vX-vO + gs
-- ================================================================================
--findGS :: ([[Char]] a)=> a->a ->Int->Int
findGS a b gl =
            let z = length (filter(\r -> (length r) == gl) a )
                q = length (filter(\z -> (length z) == gl) b )
                ts = (z+ q) == gl
                x = (z==gl)
                y= (q==gl)
            in
              if x then (10^gl)
              else
                  if y then -1*(10^gl)
                  else
                      if ts then 1
                      else 0


-- ================================================================================
maximum' :: Ord a => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n < (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps
-- ================================================================================
createBoard :: Board -> [[Char]]
createBoard x =
               let
                  z = genList x
                  n = gameSize x
                  w = [drop ((x-1)*n) (take (x*n) z)| x<- [1..n]]
                in w
-- ================================================================================
updateE :: Board -> Int ->[Int]
updateE a n =
             let
                x = empty a
                z = filter (/= n  ) x
              in z
-- ================================================================================
updateO :: Board ->Int-> Board
updateO a n=
             let
                x = qsrt (n : (listO a))
                e = updateE a n
                nG = 10 ^ (gameSize a)
                k = a{ listO = x ,empty = e}
                profit' = findPr (findP k)
                gst =  ( profit' `div`  nG)
                gs = if (gst < 0 ) then -1
                     else if (length e ==  0) then 0
                          else  2
                in  k{profit = profit',gameStatus = gs}
-- ================================================================================
updateX :: Board ->Int-> Board
updateX a n=
             let
                x = qsrt (n : (listX a))
                e = updateE a n
                as =(gameSize a )
                nG = 10 ^ as
                k = a{ listX = x ,empty = e}
                profit' = findPr (findP k)
                gst =  ( profit' `div`  nG)
                gs = if (gst > 0 ) then 1
                     else if (length e ==  0) then 0
                           else  2
                in  k{profit = profit',gameStatus = gs}

-- ================================================================================
val :: (Char,Int)-> Int
val (a,b) = b
-- ================================================================================
mySort :: [(Char,Int)] -> [(Char,Int)]->[(Char,Int)]->[(Char,Int)]
mySort (a:ax) [] [] = (a:ax)
mySort [] a [] = a
mySort [] [] a = a
mySort (a:ax) (b:bx) []
             |val a > val b = b:mySort (a:ax) bx []
             |otherwise    =  a:mySort ax (b:bx) []
mySort a b c =mySort (mySort a b  []) c []
-- ================================================================================
genList :: Board -> [(Char)]
genList x  =
           let
              xy = (map(\y -> ('X',y))(listX x ))
              yz=  (map(\y -> ('O',y))(listO x ))
              e = (map(\y -> (' ',y))(empty x ))
              z =  mySort xy yz e
              r = map (\ (a,b) -> a ) z
           in r
-- ================================================================================
data Tree a   = EmptyTree | Node a (Tree a) (Tree a)  deriving (Read, Show)
sngltn :: Board -> Tree Board
sngltn x = Node x EmptyTree EmptyTree
treeIns :: Board  -> Tree Board  -> Tree Board
treeIns x EmptyTree = sngltn x
treeIns x (Node a left right)
       | profit x == profit a = Node x left right
       | profit x > profit a  =  Node a left (treeIns x right)
       | otherwise  = Node a (treeIns x left) right
-- ================================================================================
qsrt [] = []
qsrt (x:xs) = small++ [x] ++large
     where
        small = [y| y<- xs , y < x]
        large = [y| y<- xs , y > x]
-- ================================================================================
myGetLine :: IO String
myGetLine  = do
               x <- getChar
               if ( x == '\n') then return []
               else
                   do  xs <- myGetLine
                       return (x:xs)
-- ================================================================================
getInt :: String -> IO Int
getInt s =
           do
              putStr s
              x<-myGetLine
              let k = x !! 0
              let z = read [k]::Int
              return z
-- ================================================================================
myList n =  [0..(n-1)]
-- ================================================================================

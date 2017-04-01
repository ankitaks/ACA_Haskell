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
      ,gameStatus:: Char
      ,gameSize :: Int
     } deriving (Show)
                          ------Board Operations
-- ================================================================================
main =
 do
   n <- getInt " Enter Size of the Board  "
   if(not (checkSize n)) then do putStrLn "Invalid Size "
   else do
         let k =  Board{profit=0,gameSize = n, gameStatus= 'C', listX =[],listO =[],empty = [1..(n*n)]}
         playGame k False -- True for cpu to play first
-- ================================================================================
first (a,_) = a
secnd (a,b) = b
checkSize n =  n>2 && n<= 10
assignMin :: Board -> Board
assignMin board1 =
                  let rB = board1{profit = -1000000000}
                  in rB
-- ================================================================================
addSpace :: [Char] -> [Char]
addSpace [] = []
addSpace a = a
display :: [Char] -> Int -> IO()
display [] n = putStrLn " "
display xs n =
 do
      let y =  (take n xs)
      putStrLn (addSpace y)
      display  (drop n xs)  n
-- ================================================================================
playGame b1 cpuTurn  =
 do
          let n =  gameSize b1
          if(cpuTurn)
              then
              do
                putStrLn "CPUs turn"
                let
                    b3 = [updateX b1 y |y <- empty b1]
                    b2 = first $ maximum' $map(\x -> (x,minimax x 0 False) ) b3
                    gs = gameStatus b2
                display (genList b2) (n)
                if(gs == 'X')then do putStrLn " CPU Wins !!"
                else
                    if(gs == 'T' ) then do putStrLn "Its Tie"
                     else
                      do
                         playGame b2 False
          else
                do
                  pos<- getInt "Enter position of O "
                  if (inValidate pos n ||not (elem pos (empty b1)) ) then do playGame b1 False
                  else
                    do
                      let  b2 = updateO b1 pos
                           gs = gameStatus b2
                      display (genList b2) (n)
                      if ( gs == 'O')
                       then do putStrLn "You Won!!"
                      else
                       if (gs == 'T')
                       then do putStrLn "Tie!!"
                        else do playGame b2 True
-- ================================================================================
inValidate :: Int -> Int -> Bool
inValidate i n = ( i <1 && i>(n^2) )
-- ================================================================================
minimax :: Board ->Int->Bool-> Int
minimax brd1 depth maximize  =
    if ( gameStatus brd1 /='C'  || depth == 10) then (profit brd1)
    else
       if(maximize)
       then   max  (profit brd1) (minimax  ( (findX brd1 'X'))  (depth+1) False)
       else   min  (profit brd1) (minimax ( (findX brd1 'O')) (depth+1)  True)
-- ==================================================================================
minimum' :: (Board,Int)->(Board,Int)->(Board,Int)
minimum' a b
    |secnd a > secnd b = b
    |otherwise = a
-- ================================================================================
findX :: Board ->Char-> Board
findX x  player=
               let
                  e = empty x
                  xb = [update player x y|y<-e]
                  in  if(length xb == 0 ) then  x
                  else  let
                            sign = if(player=='O')then (-1)  else 1
                            xB =first $ maximum' $ map (\z -> (z, sign* (profit z)) ) xb
                        in  xB
-- ================================================================================
update player board n
                  |gameStatus board == 'T'  = board
                  |player == 'O' = updateO board n
                  |otherwise  = updateX board n
-- ================================================================================
data Profit = Profit
        {
          pX :: [[Char]]
          ,pO :: [[Char]]
          ,gS :: Char
        } deriving (Show)
-- ================================================================================

findP :: Board ->Char-> Profit
findP board  player =
        let   gameMat =  createBoard board
              n  = gameSize board
              length_empty = length $ empty board
              gameMat2 = transpose gameMat
              diag1 =group( [(gameMat!!i)!! i | i<-[0..(n-1)]])
              diag2 =group ( [(gameMat!!i)!!(n-1-i)|i<-[0..(n-1)]])
              row_groupedEle = foldl (\acc x -> acc ++ x) [] (map(\x  -> group x) gameMat )
              col_groupedEle = foldl (\acc x -> acc ++ x) [] (map (\x -> group x) gameMat2)
              z =  replicate n player
              isFull = (length_empty == 0)
              inDiag = ((elem z diag1)||(elem z diag2) )
              inRow =  (elem z row_groupedEle)
              inCol = elem z col_groupedEle
              gs = if(inDiag || inRow || inCol) then player
                   else if( isFull) then 'T'
                   else 'C'
              pl_O = filter ( \x -> (x!!0 == 'O') ) (row_groupedEle ++ col_groupedEle ++ diag1 ++ diag2)
              pl_X = filter ( \x -> (x!!0 == 'X') ) (row_groupedEle ++ col_groupedEle ++ diag1 ++ diag2)
              k = Profit{pX = pl_X,pO = pl_O,gS=gs}
        in k
-- ================================================================================
findPr :: Profit->(Int,Char)
findPr profit =
         let
            tmp = if(gS profit =='O')then -100000 else 0
            x = pX profit
            o = pO profit
            vX=foldl (\acc x -> acc + x) 0  (map (\q-> (10^(length q))) x )
            vO= foldl (\acc x -> acc + x) 0 (map (\q-> (10^(length q))) o)
          in (vX-vO +tmp , gS profit)
-- ================================================================================
maximum' :: Ord a => [(t, a)] -> (t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n <= (snd p) = maxTail p ps
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
                k = a{ listO = x ,empty = e}
                tmp = findPr (findP k 'O')
                profit' = first tmp
                gs = secnd tmp
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
                tmp = findPr (findP k 'X')
                profit' = first tmp
                gs = secnd tmp
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
              let z = read  x::Int
              return z
-- ================================================================================
myList n =  [0..(n-1)]
-- ================================================================================

import Data.List
data Board =Board{
             gameSize :: Int
             ,gameStatus :: Char
             ,player:: Char
             ,oppo :: Char
             ,listE :: [Int]
             ,listP :: [Int]
             ,listO :: [Int]
           --  ,profit :: Int
}  deriving (Show )
-- ======================================================
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
-- ======================================================
max' [a] = a
max' (a:b) =max a $max' b
min' [a] = a
min' (a:b) =min a $min' b
-- ======================================================
isValid :: Int->Int->Bool
isValid i n
           |i >0 && i<= (n^2) =True
           |otherwise = False
-- ======================================================
isValid' n
         |n>0&&n<=10 = True
         |otherwise = False
-- ======================================================
main  =
   do
        n<- getInt "Enter Board Size!!"
        let board = gameInit n  'X' -- X is CPU
        playG board 'O'



 -- ======================================================
flipI:: Char -> String
flipI q
      |q=='W' = "CPU Wins"
      |q=='D' = "You Win"
      |q=='T' = "Its a Tie"
      |otherwise = ""
--playG :: Board->Char->[Char]
playG board player'=
 do
     if(player' == player board)
       then
          do
             putStrLn " CPU s Turn"
             let
                 z  =  [updateP board x|x<-(listE board)]
                 x  = fL (maximum'(map(\y->(y,minimax y 0 False)) z) )
                 xs = concat$groupList (gameList x) (gameSize x)
             display xs (gameSize board)
             if((hasMoves x) ) then playG x (flip' player')
             else
                do
                  putStrLn $ flipI (gameStatus x)

      else
           do
                n<-getInt "Enter the O "
                let z= updateO board n
                    xs = concat$groupList (gameList z) (gameSize z)
                display xs (gameSize board)
                if((hasMoves z) ) then playG z (flip' player')
                else
                   do
                     putStrLn $ flipI (gameStatus z)
                    -- -}
-- ======================================================

minimax :: Board ->Int->Bool->Int
minimax board depth isPlayer =
   if(hasMoves board && depth < 3)
      then let
               moves -- list of valid player|opponent moves
                    |(isPlayer) = [updateP board n | n<-(listE board)]
                    |otherwise =  [updateO board n | n<-(listE board)]
               set = map(\x-> (minimax (x) (depth+1) (not isPlayer))) moves
               min_maxBrd
                    |isPlayer =  max' set
                    |otherwise = min' set
           in (min_maxBrd)
      else  profit board
-- ======================================================
profit :: Board -> Int
profit board =
    let
       n=gameSize board
       rowMat=groupList (gameList board) (gameSize board)
       y' =  concat $(map(\x->group x)rowMat )
       d1 = group [(rowMat !!q)!!q| q<-[0..(n-1)]]
       d2 = group [(rowMat!!(q-1))!!(n-q)|q<-[1..n]]
       y'' =  concat $map(\x->group x)(transpose rowMat)
       r= map(\x -> (sign (x!!0) board)*(10^(length x))) (y' ++ y'' ++ d1 ++ d2)
       a = (foldl (+) 0 r) + if(gameStatus board== 'D') then  (-1000000) else 0
      in a
-- ======================================================
groupList x n = [ (take (n) (drop ((y-1)*n)  x))| y <-[1..n]]
sign :: Char-> Board-> Int
sign player' board
               |player'  == player board =1
               |player' == '_' = 0
               |otherwise = -1
flip' player
         |player== 'X' = 'O'
         |otherwise = 'X'
hasMoves board = (gameStatus board ==  'U')
-- ======================================================
winStatus player' mat n =
       let
          w=replicate n player'
          inRow = elem w mat
          inCol = elem w (transpose mat)
          d1 = [[(mat!!q)!!q| q<-[0..(n-1)]]]
          d2 = [[(mat!!(q-1))!!(n-q)|q<-[1..n]]]
          inD1 =  (elem w d1)
          inD2 =  (elem w d2)
        in (inCol || inD1 ||inD2||inRow)
-- ======================================================
updateStatus player' board =  -- working correctly
   let
          n=gameSize board
          y=groupList (gameList board) (gameSize board) -- gameMatrix
          gs = (winStatus player' y n )
    in gs
-- ======================================
gameInit n player'  = Board{listO=[],listP=[],listE=[1..(n^2)],gameSize=n,gameStatus='U',player=player',oppo=flip' player'}
-- ======================================
gameList board =
              let
                 x = map(\x-> (player board , x) ) $ qsort  $ listP board
                 y = map(\x-> (oppo board, x))$ qsort  $  listO board
                 e = map(\x -> ('_' ,x)) $ qsort  $ listE board
                 z = map(\x->fL x)$  merge x ( merge y e)
               in z
-- ========================
updateO :: Board-> Int -> Board
updateO board n  =
           let
              x =  n : listO board
              y = filter (\x -> (x/=n)) (listE board)
              board' = board{listO =x,listE = y}
              g1  = updateStatus (oppo board) board'
              gs = if(g1) then  'D' else if(length y == 0)then 'T' else 'U'
              k = Board{ listO = x,listE = y, listP= (listP board),player = (player board) , oppo  = (oppo board), gameSize =(gameSize board), gameStatus = gs}
             in k
-- ========================
updateP :: Board-> Int -> Board
updateP board n  =
               let
                  x =  n : listP board
                  y = filter (\x -> (x/=n)) (listE board)
                  board' = board{listP =x,listE = y}
                  g1 = updateStatus (player board) board'
                  gs = if(g1) then  'W' else if(length y == 0)then 'T' else 'U'
                  k = Board{ listO = (listO board),listE = y, listP= x,player = (player board) , oppo  = (oppo board), gameSize =(gameSize board), gameStatus = gs}
                in k
-- =========================
merge [] a = a             -- OK
merge a [] = a
merge (a:ab) (b:ba)
          |sL a > sL b = b : merge (a:ab) ba
          |otherwise  =  a :merge ab (b:ba)
-- =========================
sL ( a, b) = b         -- OK
fL  (a,b) = a           -- OK
-- =========================
qsort [] =[]                 -- OK
qsort (x:xs) = (qsort small) ++[x] ++ (qsort  large)
             where
                 small = [y |y<-xs,y<x]
                 large = [z |z<-xs,z>x]
-- ==========================          -- OK
maximum' :: Ord a => [(t, a)] -> ( t, a)
maximum' []     = error "maximum of empty list"
maximum' (x:xs) = maxTail x xs
  where maxTail currentMax [] = currentMax
        maxTail (m, n) (p:ps)
          | n <= (snd p) = maxTail p ps
          | otherwise   = maxTail (m, n) ps
-- ==========================          -- OK
minimum' :: Ord a => [(t, a)] -> (t, a)
minimum' []     = error "minimum of empty list"
minimum' (x:xs) = minTail x xs
  where minTail currentMin [] = currentMin
        minTail (m, n) (p:ps)
          | n > (snd p) = minTail p ps
          | otherwise   = minTail (m, n) ps
-- ===============================================
myGetLine :: IO String      -- OK
myGetLine  = do
               x <- getChar
               if ( x == '\n') then return []
               else
                   do  xs <- myGetLine
                       return (x:xs)
getInt :: String -> IO Int  -- OK
getInt s =
           do
              putStr s
              x<-myGetLine
              let z = read  x::Int
              return z
-- ===============================================

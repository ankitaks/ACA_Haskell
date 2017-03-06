-- hangman:
--One player secretly types in a word.
--The other player tries to deduce the word, by entering
---a sequence of guesses.
---For each guess, the computer indicates which letters in
--the secret word occur in the guess.
---  The game ends when the guess is correct.
import System.IO
myGetChar :: IO Char
-- myGetChar = a  function that takes a input from stdin without printing       it on the console
myGetChar  = do
               hSetEcho stdin False --makes the input invisible on StdOut
               c <- getChar 
               hSetEcho stdin True --makes the input visible on StdOut
               return c
myGetLine :: IO String 
--myGetLine  = a function that reads the input 
myGetLine = do 
              x <- myGetChar
              if x == '\n' then do putChar x
                                   return []
              else 
                  do  putChar '*'
                      xs <- myGetLine 
                      return (x:xs)
part :: String -> String -> String
part xs ys
           |length xs >=  length ys  = [if elem x ys then x else '*' | x <- xs ]
           |otherwise  = "Try Again!!"
guessW :: String -> IO ()
guessW wrd = 
            do 
               putStr "> "
               xs <- getLine 
               if xs  == wrd then 
                              putStrLn "You got it :-) "
                   else
                      do 
                          putStrLn ( part wrd xs)
                          guessW wrd
                  
hangman :: IO ()
hangman =  
       do putStrLn "Enter a word: "
          word <- myGetLine
          putStrLn "Its time to Guess the word !! : "
          guessW word

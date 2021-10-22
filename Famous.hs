module Main where 
import System.IO.Error

data QA = Q String QA QA | Ans String
    deriving (Show, Read)

tree :: QA
tree = (Q "Is she from Europe? " (Q "Is she a scientist? " (Ans "Marie Curie") (Ans "Queen Elibeth II") ) (Q "Is she an actress?" (Ans "Marilyn Monroe") (Ans "Hillary Clinton") ))       

question :: String -> IO String
question que = do
    putStr que
    getLine

yesNoQ :: String -> IO Bool
yesNoQ que = do
    ans <- question que 
    if ans == "yes" then return True
    else if ans == "no" then return False
    else do 
        putStrLn ("Please type yes or no ") 
        yesNoQ que

play :: QA -> IO QA
play (Ans a) = do 
    ans <- yesNoQ ("Is it " ++ show a ++ "? ")
    if (ans) then do
         putStrLn ("Woho, I win")
         return (Ans a)
    else do
        putStrLn ("Ok - you win this time")
        person <- question ("Just curious: Who was your famous person? ") 
        newquestion <- question ("Give me a question for which the answer for " ++ person ++ " is yes and the answer for " ++ a ++ " is no: " )
        return (Q newquestion (Ans person) (Ans a))
        
       
play (Q que yes no) = do
    ans <- yesNoQ que
    if ans == True then do
        newyes <- play yes
        return (Q que newyes no)
    else do
        newno <- play no 
        return (Q que yes no)
       

main :: IO ()
main = do
    tryError <- tryIOError (readFile "famous.qa")
    case tryError of
      Left _ -> do
       newtree <- play tree
       writeFile "famous.qa" (show newtree)
       playAgain
      Right _ -> do
       readTheFile <- readFile "famous.qa"
       newTree <- play (read readTheFile)
       writeFile "famous.qa" (show newTree)
       playAgain

playAgain = do doYou <- yesNoQ "Do you want to play again?"
               if doYou then (do 
                 main) else (do
                 putStrLn "Have a good day :)")

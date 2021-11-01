-- Authors: Albin Boklund, Linus Lindström, Isak Nordin
-- Group: 12

module Main where 
import System.IO.Error

data QA = Q String QA QA | Ans String
    deriving (Show, Read)


-- Creates a decision tree, the first one, before the computer asks you any more questions. 
tree :: QA
tree = (Q "Is she from Europe? " (Q "Is she a scientist? " (Ans "Marie Curie") (Ans "Queen Elibeth II") ) (Q "Is she an actress?" (Ans "Marilyn Monroe") (Ans "Hillary Clinton") ))       

-- Takes in a question and you type in a answer. 
question :: String -> IO String
question que = do
    putStr que
    getLine

-- checks if the answer from the function question is yes or no or something else, if yes then yesNoQ returns True, if no then yesNoQ returns False, and if something else then you need o type in a new answer. 
yesNoQ :: String -> IO Bool
yesNoQ que = do
    ans <- question que 
    if ans == "yes" then return True
    else if ans == "no" then return False
    else do 
        putStrLn ("Please type yes or no ") 
        yesNoQ que

-- if QA is a answer, then first asks you if it is a person, if yes then the computer won, if no then you win and the computer asks you for a question which the anser for your person is yes and no for the other one, 
--   then returns the new question as answer for the question before. 
-- if QA is a question, you play the "answer" for that question. 
-- This keeps going untill you reach the end of a branch. 
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
    if ans 
    then do
        newyes <- play yes
        return (Q que newyes no)
    else do
        newno <- play no 
        return (Q que yes newno)

-- plays the game with the decisiontree from "famous.txt". and with help from all the other functions, we can play threw the game. 
main :: IO ()
main = do
    qa <- readQAFile "famous.txt"
    newtree <- play qa
    writeFile "famous.txt" (show newtree)
    doYou <- yesNoQ "Do you want to play again? " 
    if doYou 
        then main
    else putStrLn "Have a good day :)"

-- 
readQAFile :: FilePath -> IO QA
readQAFile filepath = do
    tryError <- tryIOError (readFile filepath)
    case tryError of
        Left _ -> return tree
        Right x -> return (read x)

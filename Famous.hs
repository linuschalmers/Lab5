
data QA = Q String QA QA | Ans String
    deriving (Show, Read)

que :: String
que = "Is she from Europe"

que1 :: String
que1 = "Is she a scientist?"

que2 :: String
que2 = "Is sha an actress?"

tree = (Q "Is she from Europe? " (Q "Is she a scientist? " (Ans "Marie Curie") (Ans "Queen Elibeth II") ) (Q "Is she an actress?" (Ans "Marilyn Monroe") (Ans "Hillary Clinton") ))

{-
tree = 
   question "Is she from Europe?"
        if (yesNoQ == True) then
            question "Is she a scientist?"
                if (yesNoQ == True) then
                 QA Ans "Marie Curie"
                else QA Ans "Queen Elisabeth II"
        else if (yesNoQ == False) then 
             question "Is she an actress?"
                 if (yesNoQ == True) then
                     QA Ans "Marilyn Monroe"
                 else QA Ans "Hillare Clinton"
        else ":)"
-}


            

question :: String -> IO String
question que = do
    putStr que
    getLine

yesNoQ :: String -> IO Bool
yesNoQ que = do
    ans <- question que 
    if head ans == 'y' then return True
    else return False


play :: QA -> IO QA
play (Ans a) = do 
    ans <- yesNoQ ("Is it " ++ show a ++ "? ")
    if (ans) then 
         putStrLn ("Woho, I win")
    else do
        putStrLn ("Ok - you win this time")
        putStrLn ("Just curious: Who was your famous person?")
        let person = getLine 
        putStrLn ("Give me a question for which the answer for " ++  ++ " is yes and the answer for " ++ person ++ " is no" )
        -- newQuestion (Q que que1 que2)
play (Q que que1 que2) = do
    ans <- yesNoQ que
    if ans == True then play que1
    else play que2   

{-
newQuestion :: Bool -> String
newQuestion ans 
    if (ans == False) then
         question ("Just curious: Who was your famous person?")
    else 
        putStrLn("Hi")
  

newQuestion :: QA -> IO QA  
newQuestion (Q que que1 que2)= do
    
   -}

       

--main :: IO ()
    

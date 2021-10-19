data QA = Q String QA QA | Ans String
    deriving (Show, Read)

que :: String
que = "Is she from Europe"

que1 :: String
que1 = "Is she a scientist?"

que2 :: String
que2 = "Is sha an actress?"

tree = (Q "Is she from Europe?" (Q "Is she a scientist?" (Ans) (Ans) ) (Q "Is she an actress?" (Ans) (Ans) ) )

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
    if head ans ==  'y' then return True
    else return False

play :: QA -> IO ()
play (Ans a) = do 
    putStrLn ("Is it " ++ show a ++ "?")
play (Q que que1 que2) = do
    ans <- yesNoQ que
    if ans == True then play que1
    else play que2   




--main :: IO ()
    
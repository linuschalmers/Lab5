data QA = Q String QA QA | Ans String
    deriving (Show, Read)

tree =  (Q "Is she from Europe? " (Q "Is she a scientist? " (Ans "Marie Curie") (Ans "Queen Elibeth II")) 
        (Q "Is she an actress?" (Ans "Marilyn Monroe") (Ans "Hillary Clinton") ))
            
questionAnswer :: String -> IO String
questionAnswer que = do
    putStr que
    getLine

yesNoQ :: String -> IO Bool
yesNoQ que = do
    ans <- questionAnswer que 
    if head ans == 'y' then return True
    else return False

play :: QA -> IO QA
play (Ans a) = do 
    ans <- yesNoQ ("Is it " ++ show a ++ "? ")
    if (ans) then do
         putStrLn ("Woho, I win")
         return (Ans a)
    else do
        putStrLn ("Ok - you win this time")
        person <- questionAnswer ("Just curious: Who was your famous person? ") 
        newquestion <- questionAnswer ("Give me a question for which the answer for " ++ person ++ " is yes and the answer for " ++ a ++ " is no: " )
        return (Q newquestion (Ans person) (Ans a))
play (Q que yes no) = do
    ans <- yesNoQ que
    if ans == True then do
        newyes <- play yes
        return (Q que newyes no)
    else do
        newno <- play no
        return (Q que yes newno)





--main :: IO ()
    

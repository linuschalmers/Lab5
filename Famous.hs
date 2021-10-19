
data QA = Q String QA QA | Ans String
    deriving (Show, Read)

que :: String
que = "Is she from Europe"

que1 :: String
que1 = "Is she a scientist?"

que2 :: String
que2 = "Is sha an actress?"

tree = Q "Is she from Europe?" (Q "Is she a scientist?" (Ans ) (Ans)) (Q "Is sha an actress?" (Ans) (Ans))

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
play (Q que que1 que2) = do
    ans <- yesNoQ que
    if ans == True then que1
    else que2    



--main :: IO ()
    



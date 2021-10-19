

data QA = Q String QA QA | Ans String
    deriving (Show, Read)


question :: String -> IO String
question que = do
    putStr que
    getLine


yesNoQ :: String -> IO Bool
yesNoQ que = do 
    if head que ==  'y' then return True
    else return False
    



--main :: IO ()
    



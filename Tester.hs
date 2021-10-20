
{-
data Answer = Yes | No 

data Tree a = Empty | Branch a Answer Answer  
 deriving (Show, Read) 
-}


hw :: IO ()
hw = putStrLn "HelloWorld"

greet :: IO ()
greet = do 
    putStrLn "What is your name?"
    name <- getLine 
    putStrLn ("Hello " ++ name ++ ".")
    
{-
greet2 :: IO ()
greet2 = do 
    putStrLn "What is your name?"
    name <- getLine
    let uname = map toUpper (name)
    putStrLn ("Hello " ++ uname ++ ".")
-}

main :: IO ()
main = do 
    i <- getLine 
    if i /= "quit" then do
        putStrLn ("Input: " ++ i)
        main
    else 
        return ()

count :: Int -> Int -> IO ()
count n m = do 
    putStrLn (show n) 
    if n < m then 
        count (n+1) m 
    else 
        return() 

question :: IO ()
question = do
    putStrLn "Do you like candy?"
    answer <- getLine
    if answer == "Yes" then do 
        putStrLn ("Me to!")
    else putStrLn ("I do.")

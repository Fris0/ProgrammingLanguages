import System.Environment -- required for getArgs

main :: IO()
main = do
    args <- getArgs
    if null args then putStrLn "please provide command line arguments"
                 else do
                    text <- readFile (head args)
                    putStrLn text
import System.Environment (getArgs)

parseToLines :: FilePath -> IO [String]
parseToLines fp = do
  contents <- readFile fp
  let clean = filter (/= '\n')
  return $ lines contents

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of
        (x:_) -> x
        _ -> "test.txt"

  putStrLn $ "Using file: " ++ fp
  lines <- parseToLines fp
  print lines

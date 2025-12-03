import System.Environment (getArgs)
import Data.List.Split

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
  let numList = [result |
                  list <-  map (\line -> splitOn "" line) lines,
                  let numsList = map (\c -> read c::Int) (tail list),
                  let listWithIndex = zip [0..] numsList,
                  let (max, maxIndex) = maximum listWithIndex,
                  let (_, rest) = splitAt maxIndex numsList,
                  let secondNumber = if length rest == 0 then max else maximum rest,
                  let firstNumber = if length rest == 0 then maximum $ tail $ reverse numsList else max,
                  let result = firstNumber * 10 + secondNumber
                ]

  print $  numList

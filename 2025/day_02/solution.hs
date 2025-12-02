import System.Environment (getArgs)
import Data.List.Split

wordsWhen     :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseToLines :: FilePath -> IO [String]
parseToLines fp = do
  contents <- readFile fp
  let clean = filter (/= '\n')
  return $ map clean $ splitOn "," contents

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of
        (x:_) -> x
        _ -> "input.txt"

  putStrLn $ "Using file: " ++ fp
  lines <- parseToLines fp

  putStrLn "\n=== Part One ==="
  partOneResult <- partOne lines 0
  print partOneResult

  putStrLn "\n=== Part Two ==="
  -- let (_, result2) = partTwo lines [(50, 0)]
  -- print result2

partOne :: [String] -> Int -> IO Int
partOne [] result = return result
partOne (range:rest) result = do
  let
    [startS, endS] = splitOn "-" range
    start = read startS :: Int
    end   = read endS   :: Int
    
    acc =
      sum
      [ read (a <> b) :: Int
      | x <- [start .. end]
      , let s = show x
      , let (a, b) = splitAt (length s `div` 2) s
      , a == b
      ]

  partOne rest (result + acc)

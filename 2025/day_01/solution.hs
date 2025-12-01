import Data.List (uncons)
import Data.Maybe (fromJust)

parseToLines :: IO [String]
parseToLines = do
  contents <- readFile "input.txt"
  let ls = lines contents
  return ls

partOne :: [String] -> [Int] -> ([Int], Int)
partOne [] results = (results, length (filter (== 0) results))
partOne (line:rest) results =
  let
    (d:nStr) = line
    n = read nStr :: Int
    curr = case results of
             []    -> 0
             (x:_) -> x

    acc = case d of
            'L' -> curr - n
            'R' -> curr + n
            _   -> curr

    newResults = mod acc 100 : results
  in
    partOne rest newResults

import System.Environment (getArgs)

parseToLines :: FilePath -> IO [String]
parseToLines fp = do
  contents <- readFile fp
  return (lines contents)

main :: IO ()
main = do
  args <- getArgs
  let fp = case args of
        (x:_) -> x
        _ -> "input.txt"

  putStrLn $ "Using file: " ++ fp
  lines <- parseToLines fp

  putStrLn "\n=== Part One ==="
  let (_, result1) = partOne lines [50]
  print result1

  putStrLn "\n=== Part Two ==="
  let (_, result2) = partTwo lines [(50, 0)]
  print result2

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

partTwo :: [String] -> [(Int, Int)] -> ([(Int, Int)], Int)
partTwo [] results =
  (results, sum (map snd results))

partTwo (line:rest) results =
  case line of
    (d:nStr) ->
      let
        n    = read nStr :: Int
        curr = case results of
                 []     -> 50
                 (x:_)  -> fst x

        count =
          case d of
            'R' -> (curr + n) `div` 100
            'L' -> if curr == 0
                     then n `div` 100
                     else if n < curr
                       then 0
                       else 1 + (n - curr) `div` 100
            _   -> 0

        delta = case d of
                  'R' -> n
                  'L' -> -n
                  _   -> 0
        newPos = (curr + delta) `mod` 100

        newResults = (newPos, count) : results
      in
        partTwo rest newResults

    _ -> error ("Bad input line: " ++ show line)

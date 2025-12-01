fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib(n - 1) + fib(n - 2)

sumList :: [Int] -> Int
sumList list
  | null list = -1
  | otherwise =
    sum . map (*2) $ list


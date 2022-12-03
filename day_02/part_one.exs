File.read!("./day_02/input.txt")
|> String.split("\n")
|> Enum.map(fn
    "A Y" -> 8
    "A X" -> 4
    "A Z" -> 3
    "B Y" -> 5
    "B X" -> 1
    "B Z" -> 9
    "C Y" -> 2
    "C X" -> 7
    "C Z" -> 6
    _ -> 0
end)
|> Enum.sum()
|> IO.inspect()
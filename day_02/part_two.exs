File.read!("./day_02/input.txt")
|> String.split("\n")
|> Enum.map(fn
    "A Y" -> 4
    "A X" -> 3
    "A Z" -> 8
    "B Y" -> 5
    "B X" -> 1
    "B Z" -> 9
    "C Y" -> 6
    "C X" -> 2
    "C Z" -> 7
    _ -> 0
end)
|> Enum.sum()
|> IO.inspect()
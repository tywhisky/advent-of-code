File.read!("./day_01/input.txt")
|> String.split("\n\n")
|> Enum.map(fn str ->
    str
    |> String.split("\n")
    |> Enum.map(fn 
        "" -> 0
        num -> String.to_integer(num)
    end)
    |> Enum.sum()
end)
|> Enum.sort(:desc)
|> Enum.take(3)
|> Enum.sum()
|> IO.inspect()
downcase = 
    ?a..?z
    |> Enum.to_list
    |> List.to_string
    |> String.graphemes()
    |> Enum.with_index(1)

uppercase = 
    ?A..?Z
    |> Enum.to_list
    |> List.to_string
    |> String.graphemes()
    |> Enum.with_index(27)

letter_map = 
    downcase ++ uppercase
    |> Map.new()

File.read!("./day_03/input.txt")
|> String.split("\n")
|> Enum.chunk_every(3)
|> Enum.flat_map(fn group -> 
    [first, second, third] = Enum.map(group, &Enum.frequencies(Enum.uniq(String.graphemes(&1))))
    Map.merge(first, second, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.merge(third, fn _k, v1, v2 -> v1 + v2 end)
    |> Map.filter(& elem(&1, 1) == 3)
    |> Enum.map(&elem(&1, 0))
end)
|> Enum.map(&Map.get(letter_map, &1))
|> Enum.sum()
|> IO.inspect()
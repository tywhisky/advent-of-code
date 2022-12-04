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
|> Enum.flat_map(fn content -> 
    {first, second} = String.graphemes(content) |> Enum.split(div(String.length(content), 2))
    first -- (first -- second) |> Enum.uniq()
end)
|> Enum.map(&Map.get(letter_map, &1))
|> Enum.sum()
|> IO.inspect()
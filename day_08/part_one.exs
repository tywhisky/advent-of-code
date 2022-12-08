defmodule Day08.PartOne do
  def run(tuple, x, y, result) when x == 0 do
    {v, k} = elem(tuple, x) |> elem(y)
    new_tuple = tuple |> Tuple.delete_at(x, y) |> Tuple.insert_at(x)
    run(tuple, x, y + 1, result + 1)
  end
end

File.read!("./day_08/input.txt")
|> String.split("\n")
|> Enum.map(fn list ->
  list
  |> String.graphemes()
  |> Enum.map(&{String.to_integer(&1), false})
  |> List.to_tuple()
end)
|> List.to_tuple()
|> Day08.PartOne.run()
|> IO.inspect()

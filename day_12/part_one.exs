defmodule Day12.PartOne do
  def find_start(height_map, x, y) when y >= height_map_size(elem(height_map, 0)) do
    find_start(height_map, x + 1, 0)
  end

  def find_start(height_map, x, y) do
    case elem(height_map, x) |> elem(y) do
      "S" -> {x, y}
      _ -> find_start(height_map, x, y + 1)
    end
  end

  def run(height_map, {x, y}, count) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}] 
    |> Enum.map(fn 
        {nx, ny} when nx < 0 or ny < 0 -> count
        {nx, ny} when nx >= tuple_size(height_map) or ny >= tuple_size(elem(height_map, 0)) ->
            count
        {nx, ny} when elem(elem(height_map, nx), ny) == "." ->
            count
        {nx, ny} when elem(elem(height_map, nx), ny) == "E" -> 
            count + 1
        {nx, ny} ->
                new_row = 
                height_map
                |> elem(x)
                |> Tuple.delete_at(y)
                |> Tuple.insert_at(y, ".")
            
                new_height_map = 
                    height_map
                    |> Tuple.delete_at(x)
                    |> Tuple.insert_at(x, new_row)

            
        
    end)
  end
end

input =
  File.read!("./day_12/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.graphemes(&1))
  |> Enum.map(&List.to_tuple(&1))
  |> List.to_tuple()

start_point = Day12.PartOne.find_start(input, 0, 0)

Day12.PartOne.run(input, start_point, 0)
|> IO.inspect()
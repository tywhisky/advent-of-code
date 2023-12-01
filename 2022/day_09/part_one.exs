defmodule Day09.PartOne do
  def run([], _, record), do: record

  def run(["R" | tail], {t_x, t_y, h_x, h_y}, record) do
    points = follow(t_x, t_y, h_x + 1, h_y)
    run(tail, points, MapSet.put(record, {elem(points, 0), elem(points, 1)}))
  end

  def run(["D" | tail], {t_x, t_y, h_x, h_y}, record) do
    points = follow(t_x, t_y, h_x, h_y - 1)
    run(tail, points, MapSet.put(record, {elem(points, 0), elem(points, 1)}))
  end

  def run(["L" | tail], {t_x, t_y, h_x, h_y}, record) do
    points = follow(t_x, t_y, h_x - 1, h_y)
    run(tail, points, MapSet.put(record, {elem(points, 0), elem(points, 1)}))
  end

  def run(["U" | tail], {t_x, t_y, h_x, h_y}, record) do
    points = follow(t_x, t_y, h_x, h_y + 1)
    run(tail, points, MapSet.put(record, {elem(points, 0), elem(points, 1)}))
  end

  def follow(t_x, t_y, h_x, h_y) when abs(h_x - t_x) <= 1 and 
    abs(h_y - t_y) <= 1 do
    {t_x, t_y, h_x, h_y}
  end

  def follow(t_x, t_y, h_x, h_y) do
    {compute(h_x - t_x) + t_x, compute(h_y - t_y) + t_y, h_x, h_y} 
  end
  
  defp compute(0), do: 0
  defp compute(n), do: n |> abs() |> div(n) 
end

File.read!("./day_09/input.txt")
|> String.split("\n")
|> Enum.map(&String.split(&1, " "))
|> Enum.flat_map(fn [action, step] ->
  List.duplicate(action, String.to_integer(step))
end)
|> Day09.PartOne.run({0, 0, 0, 0}, MapSet.new([{0, 0}]))
|> MapSet.to_list()
|> length()
|> IO.inspect()

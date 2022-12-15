defmodule Day14.PartTwo do
  def build_wall([], _prev, map), do: map

  def build_wall([[x, y] | tail], nil, map) do
    build_wall(tail, [x, y], map)
  end

  def build_wall([[x, y] | tail], [p_x, y], map) do
    new_map =
      for c_x <- p_x..x do
        {{c_x, y}, :stone}
      end
      |> Map.new()
      |> Map.merge(map)

    build_wall(tail, [x, y], new_map)
  end

  def build_wall([[x, y] | tail], [x, p_y], map) do
    new_map =
      for c_y <- p_y..y do
        {{x, c_y}, :stone}
      end
      |> Map.new()
      |> Map.merge(map)

    build_wall(tail, [x, y], new_map)
  end

  def run({x, y}, walls) do
    if walls[{500, 0}] do
      walls
    else
      case {walls[{x, y + 1}], walls[{x - 1, y + 1}], walls[{x + 1, y + 1}]} do
        {nil, _, _} ->
          run({x, y + 1}, walls)

        {_, nil, _} ->
          run({x - 1, y + 1}, walls)

        {_, _, nil} ->
          run({x + 1, y + 1}, walls)

        _ ->
          new_walls = Map.put(walls, {x, y}, :sand)
          run({500, 0}, new_walls)
      end
    end
  end
end

walls =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.split(&1, "->"))
  |> Enum.map(
    &Enum.map(&1, fn str ->
      String.replace(str, " ", "", global: true)
      |> String.split(",")
      |> Enum.map(fn
        "" -> []
        x -> String.to_integer(x)
      end)
    end)
  )
  |> List.delete_at(-1)
  |> Enum.reduce(%{}, fn list, record ->
    Day14.PartTwo.build_wall(list, nil, record)
  end)

{{_, lowest}, _} = Enum.max_by(walls, &elem(elem(&1, 0), 1))

walls =
  for bottom <- (-lowest - 10)..(lowest + 10) do
    {{bottom + 500, lowest + 2}, :stone}
  end
  |> Map.new()
  |> Map.merge(walls)

Day14.PartTwo.run({500, 0}, walls)
|> Enum.filter(&(elem(&1, 1) == :sand))
|> length()
|> IO.inspect()

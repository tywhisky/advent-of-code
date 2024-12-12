Mix.install([{:libgraph, "~> 0.16.0"}])

defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, col_idx} ->
      row
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, row_idx} ->
        {{col_idx, row_idx}, c}
      end)
    end)
  end

  def part_one(path) do
    list = parse(path)
    map = Map.new(list)
    map_with_perimeter = build_map_with_perimeter(list, map, %{})

    list
    |> Enum.reduce(Graph.new(), fn {{x, y}, type}, g ->
      vertices =
        [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
        |> Enum.filter(&(map[&1] == type))

      edges =
        vertices
        |> Enum.map(fn to -> {{x, y}, to} end)

      g
      |> Graph.add_vertices([{x, y} | vertices])
      |> Graph.add_edges(edges)
    end)
    |> Graph.components()
    |> Enum.map(fn group ->
      perimeter =
        group
        |> Enum.map(&map_with_perimeter[&1])
        |> Enum.sum()

      length(group) * perimeter
    end)
    |> Enum.sum()
  end

  defp build_map_with_perimeter([], _map, result), do: result

  defp build_map_with_perimeter([{{x, y}, type} | tail], map, result) do
    perimeter =
      [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
      |> Enum.reject(&(map[&1] == type))
      |> Enum.count()

    build_map_with_perimeter(tail, map, Map.put(result, {x, y}, perimeter))
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")
# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

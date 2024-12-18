Mix.install([
  {:libgraph, "~> 0.16.0"}
])

defmodule Solution do
  def parse(path, len, bytes) do
    om =
      for i <- 0..len, j <- 0..len do
        {{i, j}, "."}
      end
      |> Map.new()

    nm =
      path
      |> File.read!()
      |> String.split(["\n", ","], trim: true)
      |> Enum.chunk_every(2)
      |> Enum.take(bytes)
      |> Map.new(fn [x, y] -> {{String.to_integer(x), String.to_integer(y)}, "#"} end)

    Map.merge(om, nm)
  end

  def part_one(path, len, bytes) do
    map = parse(path, len, bytes)

    map
    |> Map.to_list()
    |> Enum.reduce(Graph.new(), fn
      {_coor, "#"}, g ->
        g

      {{x, y}, "."}, g ->
        edges =
          [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
          |> Enum.reject(&is_nil(map[&1]))
          |> Enum.reject(&(map[&1] == "#"))
          |> Enum.map(fn to -> {{x, y}, to} end)

        Graph.add_edges(g, edges)
    end)
    |> Graph.dijkstra({0, 0}, {len, len})
    |> length()
    |> case do
      result -> result - 1
    end
  end

  def part_two(path, len, bytes) do
    om =
      for i <- 0..len, j <- 0..len do
        {{i, j}, "."}
      end
      |> Map.new()

    {nm, rest} =
      path
      |> File.read!()
      |> String.split(["\n", ","], trim: true)
      |> Enum.chunk_every(2)
      |> Enum.map(fn [x, y] -> {{String.to_integer(x), String.to_integer(y)}, "#"} end)
      |> Enum.split(bytes)

    map = Map.merge(om, Map.new(nm))

    g =
      map
      |> Map.to_list()
      |> Enum.reduce(Graph.new(), fn
        {_coor, "#"}, g ->
          g

        {{x, y}, "."}, g ->
          edges =
            [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
            |> Enum.reject(&is_nil(map[&1]))
            |> Enum.reject(&(map[&1] == "#"))
            |> Enum.map(fn to -> {{x, y}, to} end)

          Graph.add_edges(g, edges)
      end)

    find(rest, g, len)
  end

  defp find([{{x, y}, _} | tail], g, len) do
    edges =
      [
        {{x + 1, y}, {x, y}},
        {{x - 1, y}, {x, y}},
        {{x, y + 1}, {x, y}},
        {{x, y - 1}, {x, y}},
        {{x, y}, {x + 1, y}},
        {{x, y}, {x - 1, y}},
        {{x, y}, {x, y + 1}},
        {{x, y}, {x, y - 1}}
      ]

    new_g = Graph.delete_edges(g, edges)

    case Graph.dijkstra(new_g, {0, 0}, {len, len}) do
      nil -> {x, y}
      _ -> find(tail, new_g, len)
    end
  end
end

Solution.part_one("test.txt", 6, 12) |> IO.inspect()
Solution.part_one("input.txt", 70, 1024) |> IO.inspect()
Solution.part_two("test.txt", 6, 12) |> IO.inspect()
Solution.part_two("input.txt", 70, 1024) |> IO.inspect()

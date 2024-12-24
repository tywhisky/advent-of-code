defmodule Solution do
  Mix.install([
    {:libgraph, "~> 0.16.0"}
  ])

  def parse(path) do
    path
    |> File.read!()
    |> String.split(["\n", "-"], trim: true)
    |> Enum.chunk_every(2)
    |> Enum.flat_map(fn [a, b] -> [[a, b], [b, a]] end)
  end

  def part_one(path) do
    map = parse(path) |> Enum.group_by(fn [a, _b] -> a end, fn [_a, b] -> b end)
    list = Map.to_list(map)

    group(list, map, [])
    |> Enum.filter(fn list ->
      Enum.any?(list, fn item ->
        is_binary(item) and String.starts_with?(item, "t")
      end)
    end)
    |> length()
  end

  def part_two(path) do
    edges =
      path
      |> parse()
      |> Enum.map(fn [a, b] -> {a, b} end)

    Graph.new(type: :undirected)
    |> Graph.add_edges(edges)
    |> Graph.cliques()
    |> Enum.max_by(&length/1)
    |> Enum.sort()
    |> Enum.join(",")
  end

  def group([], _map, result), do: result |> Enum.map(&Enum.sort/1) |> Enum.uniq()

  def group([{from, links} | tail], map, result) do
    new =
      for i <- links, j <- map[i] do
        if from in map[j] do
          [from, i, j]
        end
      end
      |> Enum.reject(&is_nil/1)

    group(tail, map, new ++ result)
  end
end

Solution.part_one("test.txt") |> IO.inspect()
Solution.part_one("input.txt") |> IO.inspect()
Solution.part_two("test.txt") |> IO.inspect()
Solution.part_two("input.txt") |> IO.inspect()

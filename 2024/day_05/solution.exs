Mix.install([{:libgraph, "~> 0.16.0"}])

defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> case do
      [rules, rows] ->
        rules =
          rules
          |> String.split("\n", trim: true)
          |> Enum.map(&String.split(&1, "|", trim: true))
          |> Enum.group_by(&hd(&1), &Enum.at(&1, 1))

        rows =
          rows
          |> String.split("\n", trim: true)
          |> Enum.map(&String.split(&1, ",", trim: true))

        {rules, rows}
    end
  end

  def part_one(path) do
    {map, rows} = parse(path)

    rows
    |> Enum.filter(&check(&1, [], map))
    |> Enum.map(&Enum.at(&1, div(length(&1), 2)))
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def part_two(path) do
    {map, rows} = parse(path)

    list = Map.to_list(map)

    all =
      Enum.reduce(list, [], fn {k, v}, acc -> [k | v] ++ acc end)
      |> Enum.uniq()

    order =
      list
      |> Enum.flat_map(fn {k, v} -> Enum.map(v, &{k, &1}) end)
      |> case do
        edges ->
          Graph.new() |> Graph.add_vertices(all) |> Graph.add_edges(edges)
      end
      |> Graph.topsort()

    Enum.reject(rows, &check(&1, [], map))
    |> Enum.map(fn row ->
      sort_by_order(row, order)
    end)
    |> Enum.map(&Enum.at(&1, div(length(&1), 2)))
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
  end

  def check([h | tail], [], map) do
    check(tail, [h], map)
  end

  def check([], _rest, _map), do: true

  def check([h | tail], [p | _] = rest, map) do
    if map[p] && h in map[p] do
      check(tail, [h | rest], map)
    else
      false
    end
  end

  def sort_by_order(my_list, ordered_keys) do
    index_map = Enum.with_index(ordered_keys) |> Enum.into(%{})
    Enum.sort_by(my_list, fn key -> Map.get(index_map, key, -1) end)
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with input.txt")

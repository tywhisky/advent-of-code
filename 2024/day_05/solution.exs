defmodule Solution do
  alias Solution.Sorter

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

    rows
    |> Enum.reject(&check(&1, [], map))

    list = Map.to_list(map)

    all =
      Enum.map(list, fn {k, v} -> [k | v] end)
      |> List.flatten()
      |> Enum.uniq()
      |> IO.inspect()

    Enum.reduce(list, all, fn {k, v}, acc ->
      v
      |> Enum.map(fn x -> [k, x] end)
      |> Enum.reduce(acc, fn q, a ->
        Sorter.sort_by_order(q, a)
      end)
      |> dbg()
    end)
  end

  defmodule Sorter do
    def sort_by_order(my_list, ordered_keys) do
      index_map = Enum.with_index(ordered_keys) |> Enum.into(%{})
      Enum.sort_by(my_list, fn key -> Map.get(index_map, key, -1) end)
    end
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
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with input.txt")

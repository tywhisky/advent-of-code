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

    grouped =
      list
      |> Enum.reduce({[], %{}}, fn k, {rs, re} ->
        if re[k] do
          {rs, re}
        else
          new_result = group(k, map, %{})
          new_record = Map.new(new_result, fn k -> {k, true} end) |> Map.merge(re)
          {[new_result | rs], new_record}
        end
      end)
      |> elem(0)
      |> Enum.map(&Enum.sort/1)
      |> Enum.uniq()
      |> Enum.map(fn row ->
        {length(row), Enum.map(row, &map_with_perimeter[&1]) |> Enum.sum()}
      end)
      |> Enum.map(fn {a, b} -> a * b end)
      |> Enum.sum()
  end

  defp group({{x, y}, type}, map, record) do
    record = Map.put(record, {x, y}, true)

    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.filter(&(map[&1] == type && record[&1] == nil))
    |> case do
      [] ->
        [{x, y}]

      nexts ->
        result =
          nexts
          |> Enum.map(fn x -> {x, type} end)
          |> Enum.flat_map(&group(&1, map, record))

        [{x, y} | result] |> Enum.uniq()
    end
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

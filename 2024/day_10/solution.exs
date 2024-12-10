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
        {{col_idx, row_idx}, String.to_integer(c)}
      end)
    end)
  end

  def part_one(path) do
    list = parse(path)
    map = Map.new(list)
    starts = Enum.filter(list, &(elem(&1, 1) == 0))

    Enum.map(starts, fn s ->
      move(s, map)
      |> Enum.uniq() # remove this line is partwo
      |> Enum.count()
    end)
    |> Enum.sum()
  end

  defp move({f, 9}, map), do: [f]

  defp move({{x, y}, curr}, map) do
    [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
    |> Enum.map(&{&1, map[&1]})
    |> Enum.filter(&(elem(&1, 1) == curr + 1))
    |> case do
      [] ->
        []

      nexts ->
        nexts
        |> Enum.reduce([], fn next, acc ->
           move(next, map) ++ acc
        end)
    end
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")

Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

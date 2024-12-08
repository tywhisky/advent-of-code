defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, col_idx} ->
      row
      |> String.split("", trim: true)
      |> Enum.with_index()
      |> Enum.map(fn {c, row_idx} ->
        {{col_idx, row_idx}, c}
      end)
    end)
  end

  def part_one(path) do
    list = parse(path)
    map = Map.new(list)

    group =
      path
      |> parse()
      |> Enum.reject(fn {_, c} -> c == "." end)
      |> Enum.group_by(&elem(&1, 1), &elem(&1, 0))
      |> Map.to_list()
      |> Enum.map(&elem(&1, 1))
      |> Enum.map(&expand(&1, []))
      |> Enum.map(fn row ->
        Enum.reduce(row, 0, fn r, acc ->
          acc + check(r, map)
        end)
      end)
      |> Enum.sum()
  end

  defp check([{x, y}, {a, b}], map) do
    i = {a - x + a, b - y + b}
    j = {x - a + x, y - b + y}

    case {map[i], map[j]} do
      {nil, "."} -> 1
      {".", nil} -> 1
      {".", "."} -> 2
      _ -> 0
    end
  end

  defp expand([], result), do: result

  defp expand([h | tail], result) do
    new =
      tail
      |> Enum.map(fn x -> [x, h] end)

    expand(tail, result ++ new)
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")

Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

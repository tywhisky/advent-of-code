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
      |> Enum.map(fn {c, row_idx} -> {{col_idx, row_idx}, c} end)
    end)
  end

  def part_one(path) do
    list = parse(path)
    start = Enum.find(list, fn {_, c} -> c == "^" end)
    map = Map.new(list)
    move(start, map, %{})
  end

  def move({key, _face} = point, map, result) do
    case map[key] do
      nil ->
        map_size(result)

      "#" ->
        next = find_change_next(point)
        move(next, map, result)

      _ ->
        next = find_direct_next(point)
        move(next, map, Map.put(result, key, true))
    end
  end

  defp find_change_next({{x, y}, "^"}) do
    {{x + 1, y + 1}, ">"}
  end

  defp find_change_next({{x, y}, ">"}) do
    {{x + 1, y - 1}, "v"}
  end

  defp find_change_next({{x, y}, "v"}) do
    {{x - 1, y - 1}, "<"}
  end

  defp find_change_next({{x, y}, "<"}) do
    {{x - 1, y + 1}, "^"}
  end

  defp find_direct_next({{x, y}, "^"}) do
    {{x - 1, y}, "^"}
  end

  defp find_direct_next({{x, y}, ">"}) do
    {{x, y + 1}, ">"}
  end

  defp find_direct_next({{x, y}, "v"}) do
    {{x + 1, y}, "v"}
  end

  defp find_direct_next({{x, y}, "<"}) do
    {{x, y - 1}, "<"}
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

# # Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# # Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with input.txt")

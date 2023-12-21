defmodule Day21 do
  def part_one() do
    {map, {start, _}} =
      parse()

    search(64, [start], map)
    |> Enum.uniq()
    |> length()
  end

  def search(0, result, _map), do: result

  def search(count, currs, map) do
    nexts =
      currs
      |> Enum.flat_map(fn {row_idx, col_idx} ->
        [
          {row_idx - 1, col_idx},
          {row_idx + 1, col_idx},
          {row_idx, col_idx - 1},
          {row_idx, col_idx + 1}
        ]
        |> Enum.reject(&(map[&1] == "#"))
      end)
      |> Enum.uniq()
      |> then(fn list -> list -- currs end)

    search(count - 1, nexts, map)
  end

  def parse() do
    list =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {str, row_idx} ->
        str
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {c, col_idx} -> {{row_idx, col_idx}, c} end)
      end)

    start = Enum.find(list, &(elem(&1, 1) == "S"))
    {Map.new(list), start}
  end
end

Day21.part_one() |> IO.inspect()

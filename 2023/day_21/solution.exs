defmodule Day21 do
  # def part_one() do
  #   {map, {start, _}} =
  #     parse()

  #   search(64, [start], map)
  #   |> Enum.uniq()
  #   |> length()
  # end

  def part_two() do
    {map, {start, _}} = parse()
    list = Map.to_list(map)
    rows = (Enum.filter(list, fn {{r, c}, _} -> r == 0 end) |> length()) - 1
    cols = (Enum.filter(list, fn {{r, c}, _} -> c == 0 end) |> length()) - 1

    search(50, [start], map, rows, cols)
    |> Enum.uniq()
    |> dbg()
    |> length()
  end

  def search(0, result, _map, rows, cols), do: result

  def search(count, currs, map, rows, cols) do
    nexts =
      currs
      |> Enum.flat_map(fn {row_idx, col_idx} ->
        [
          {row_idx - 1, col_idx},
          {row_idx + 1, col_idx},
          {row_idx, col_idx - 1},
          {row_idx, col_idx + 1}
        ]
        |> Enum.reject(fn {r, c} ->
          real_r =
            cond do
              r > rows -> rem(r, rows) - 1
              r < 0 -> rows - (abs(r) |> rem(rows)) + 1
              true -> r
            end

          real_c =
            cond do
              c > cols -> rem(c, cols) - 1
              c < 0 -> cols - (abs(c) |> rem(cols)) + 1
              true -> c
            end

          IO.inspect({real_r, real_c}, label: "#{r} ,#{c}")

          map[{real_r, real_c}] == "#"
        end)
      end)
      |> Enum.uniq()
      |> then(fn list -> list -- currs end)

    search(count - 1, nexts, map, rows, cols)
  end

  def parse() do
    list =
      "test.txt"
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

# Day21.part_one() |> IO.inspect()
Day21.part_two() |> IO.inspect()

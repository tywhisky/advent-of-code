defmodule Day11 do
  def part_one(times \\ 2) do
    list = parse()

    galaxies = Enum.filter(list, &(elem(&1, 1) == "#")) |> Enum.map(&elem(&1, 0))
    {empty_cols, empty_rows} = check_galaxies(list)

    find_length(galaxies, [], empty_cols, empty_rows, times)
  end

  def part_two() do
    part_one(1_000_000)
  end

  def find_length([], result, _, _, _), do: Enum.sum(result)

  def find_length([{hc, hr} | tail], result, cols, rows, times) do
    new_result =
      Enum.map(tail, fn {c, r} ->
        olength = abs(hc - c) + abs(hr - r)

        l1 = (cols -- Enum.to_list(hc..c)) |> length()
        l2 = (rows -- Enum.to_list(hr..r)) |> length()
        (length(cols) + length(rows) - l1 - l2) * (times - 1) + olength
      end)

    find_length(tail, new_result ++ result, cols, rows, times)
  end

  def check_galaxies(list) do
    empty_cols =
      Enum.group_by(list, fn {{col, _row}, _} -> col end)
      |> Enum.filter(fn {_empty_col, col_list} -> Enum.all?(col_list, &(elem(&1, 1) == ".")) end)
      |> Enum.map(&elem(&1, 0))

    empty_rows =
      Enum.group_by(list, fn {{_col, row}, _} -> row end)
      |> Enum.filter(fn {_empty_col, col_list} -> Enum.all?(col_list, &(elem(&1, 1) == ".")) end)
      |> Enum.map(&elem(&1, 0))

    {empty_cols, empty_rows}
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {str, col_idx} ->
      str
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, row_idx} -> {{col_idx, row_idx}, c} end)
    end)
  end
end

IO.puts("The Result of Day11 Part One is: #{Day11.part_one()}")
IO.puts("The Result of Day11 Part Two is: #{Day11.part_two()}")

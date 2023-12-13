defmodule Day13 do
  def part_one(), do: common(true)
  def part_two(), do: common(false)

  def common(fixed?) do
    parse()
    |> Enum.map(fn {rows, cols} ->
      row_qty = check(rows, fixed?)
      col_qty = check(cols, fixed?)

      row_qty * 100 + col_qty
    end)
    |> Enum.sum()
  end

  def check(list, fixed?) do
    list
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.with_index(1)
    |> Enum.filter(fn {[a, b], _idx} ->
      a == b or diff(a, b)
    end)
    |> Enum.map(fn {_, idx} ->
      {left, right} = Enum.split(list, idx)
      (do_check(Enum.reverse(left), right, fixed?) && idx) || 0
    end)
    |> case do
      [] -> 0
      list -> Enum.max(list)
    end
  end

  def do_check([], _, fixed?), do: fixed?
  def do_check(_, [], fixed?), do: fixed?

  def do_check([h1 | tail1], [h2 | tail2], fixed?) when h1 == h2,
    do: do_check(tail1, tail2, fixed?)

  def do_check([h1 | tail1], [h2 | tail2], fixed?) do
    if !fixed? && diff(h1, h2) do
      do_check(tail1, tail2, true)
    else
      false
    end
  end

  def diff(str1, str2) do
    [String.graphemes(str1), String.graphemes(str2)]
    |> Enum.zip()
    |> Enum.filter(fn {a, b} -> a != b end)
    |> length()
    |> case do
      1 -> true
      _ -> false
    end
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> Enum.map(fn rows ->
      cols =
        rows
        |> Enum.map(&String.graphemes/1)
        |> Enum.zip_with(&Enum.join/1)

      {rows, cols}
    end)
  end
end

IO.puts("The Result of Day13 Part One is: #{Day13.part_one()}")
IO.puts("The Result of Day13 Part Two is: #{Day13.part_two()}")

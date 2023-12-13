defmodule Day13 do
  def part_one() do
    parse()
    |> Enum.map(fn rows ->
      cols =
        rows
        |> Enum.map(&String.graphemes/1)
        |> Enum.zip_with(&Enum.join/1)

      row_qty = check(rows)
      col_qty = check(cols)

      row_qty * 100 + col_qty
    end)
    |> Enum.sum()
  end

  def part_two() do
    parse()
    |> Enum.map(fn rows ->
      cols =
        rows
        |> Enum.map(&String.graphemes/1)
        |> Enum.zip_with(&Enum.join/1)

      row_qty = check_smudge(rows) |> dbg()
      col_qty = check_smudge(cols) |> dbg()

      row_qty * 100 + col_qty
    end)
    |> Enum.sum()
  end

  def check_smudge(list) do
    list
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.with_index(1)
    |> Enum.filter(fn {[a, b], _idx} ->
      ins = String.myers_difference(a, b) |> Keyword.get(:ins)
      a == b or String.length(ins) == 1
    end)
    |> Enum.map(fn {_, idx} ->
      {left, right} = Enum.split(list, idx)
      (do_check_smudge(Enum.reverse(left), right, false) && idx) || 0
    end)
    |> case do
      [] -> 0
      list -> Enum.max(list)
    end
  end

  def do_check_smudge([], _, fixed?), do: fixed?
  def do_check_smudge(_, [], fixed?), do: fixed?

  def do_check_smudge([h1 | tail1], [h2 | tail2], fixed?) when h1 == h2,
    do: do_check_smudge(tail1, tail2, fixed?)

  def do_check_smudge([h1 | tail1], [h2 | tail2], fixed?) do
    diff =
      [String.graphemes(h1), String.graphemes(h2)]
      |> Enum.zip()
      |> Enum.filter(fn {a, b} -> a != b end)
      |> length()

    if diff == 1 && fixed? == false do
      do_check_smudge(tail1, tail2, true)
    else
      false
    end
  end

  def check(list) do
    list
    |> Enum.chunk_every(2, 1, :discard)
    |> Enum.with_index(1)
    |> Enum.filter(fn {[a, b], _idx} -> a == b end)
    |> Enum.map(fn {_, idx} ->
      {left, right} = Enum.split(list, idx)
      (do_check(Enum.reverse(left), right) && idx) || 0
    end)
    |> case do
      [] -> 0
      list -> Enum.max(list)
    end
  end

  def do_check([], _), do: true
  def do_check(_, []), do: true

  def do_check([h1 | tail1], [h2 | tail2]) when h1 == h2,
    do: do_check(tail1, tail2)

  def do_check(_, _), do: false

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
  end
end

# IO.puts("The Result of Day13 Part One is: #{Day13.part_one()}")
IO.puts("The Result of Day13 Part Two is: #{Day13.part_two()}")

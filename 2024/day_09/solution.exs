defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.replace("\n", "")
    |> String.graphemes()
    |> Enum.map(&String.to_integer/1)
    |> Enum.reduce({true, [], []}, fn
      x, {true, a, b} -> {false, [x | a], b}
      x, {false, a, b} -> {true, a, [x | b]}
    end)
    |> case do
      {_, a, b} ->
        a =
          a
          |> Enum.reverse()
          |> Enum.with_index()

        b = Enum.reverse(b)
        {a, b}
    end
  end

  def part_one(path) do
    {a, b} = parse(path)

    first = first_change(a, b, [])

    second_change(first, [])
    |> Enum.with_index()
    |> Enum.map(fn {a, b} -> a * b end)
    |> Enum.sum()
  end

  def second_change([], rest), do: Enum.reverse(rest)

  def second_change(["." | tail], rest) do
    case Enum.reverse(tail) |> pop_until() do
      [] ->
        Enum.reverse(rest)

      [h | t] ->
        second_change(Enum.reverse(t), [h | rest])
    end
  end

  def pop_until(["." | tail]), do: pop_until(tail)
  def pop_until(list), do: list

  def second_change([g | tail], rest) do
    second_change(tail, [g | rest])
  end

  def first_change([], [b], result) do
    blocks = Enum.map(1..b, fn _ -> "." end)
    Enum.reverse(blocks ++ result)
  end

  def first_change([{num, qty}], [], result) do
    spaces = Enum.map(1..num, fn _ -> qty end)
    Enum.reverse(spaces ++ result)
  end

  def first_change([{num, qty} | tail], [space | space_tail], result) do
    spaces = Enum.map(1..space, fn _ -> "." end)
    blocks = Enum.map(1..num, fn _ -> qty end)
    new_result = (spaces ++ blocks) ++ result
    first_change(tail, space_tail, new_result)
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")

Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

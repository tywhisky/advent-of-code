defmodule Solution do
  def parse() do
    path = if System.argv() == [], do: "test.txt", else: "input.txt"

    [range, ids] =
      path
      |> File.read!()
      |> String.split("\n\n", trim: true)

    range =
      range
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, "-", trim: true))
      |> Enum.map(fn [a, b] -> [String.to_integer(a), String.to_integer(b)] end)
      |> Enum.sort()

    ids =
      ids
      |> String.split("\n", trim: true)
      |> Enum.map(&String.to_integer/1)

    {range, ids}
  end

  def part_one() do
    {ranges, ids} =
      parse()

    Enum.reduce(ids, 0, fn id, result ->
      if Enum.find(ranges, fn [a, b] -> a <= id and b >= id end) != nil do
        result + 1
      else
        result
      end
    end)
    |> IO.inspect(label: "Part One Result")
  end

  def part_two() do
    {ranges, _ids} = parse()

    ranges
    |> Enum.sort()
    |> merge([])
    |> Enum.map(fn [a, b] -> b - a + 1 end)
    |> Enum.sum()
    |> IO.inspect(label: "Part Two Result")
  end

  def merge([], rest), do: Enum.reverse(rest)

  def merge([[a, b], [c, d] | tail], rest) when c <= b do
    merge([[a, max(b, d)] | tail], rest)
  end

  def merge([head | tail], rest), do: merge(tail, [head | rest])
end

Solution.part_one()
Solution.part_two()

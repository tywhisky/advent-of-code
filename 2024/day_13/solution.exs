defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn group ->
      group
      |> String.split(": ", trim: true)
      |> tl()
      |> Enum.flat_map(fn str ->
        str
        |> String.replace(~r/[X+|Y+|X=|Y=]/, "")
        |> String.split(", ", trim: true)
        |> Enum.map(&String.to_integer/1)
      end)
    end)
    |> Enum.chunk_every(3)
  end

  def part_one(path) do
    parse(path)
    |> Enum.map(&find_fewest_tokens/1)
    |> Enum.reject(&Enum.empty?/1)
    |> Enum.map(&Enum.min/1)
    |> Enum.sum()
  end

  def part_two(path) do
    parse(path)
    |> Enum.map(fn [a, b, c] -> [a, b, Enum.map(c, fn x -> x + 10_000_000_000_000 end)] end)
    |> Enum.map(&solve/1)
    |> Enum.flat_map(fn
      [a, b] -> [3 * a + b]
      _ -> []
    end)
    |> Enum.sum()
  end

  def solve([[a, c], [b, d], [e, f]]) do
    x = (d * e - b * f) / (a * d - b * c)
    y = (a * f - c * e) / (a * d - b * c)

    if floor(x) == x and floor(y) == y do
      [trunc(x), trunc(y)]
    end
  end

  defp find_fewest_tokens([[ax, ay], [bx, by], [tx, ty]]) do
    for a <- Enum.to_list(1..99), b <- Enum.to_list(1..99) do
      sum_a = a * ax + b * bx
      sum_b = a * ay + b * by
      {a, b, sum_a, sum_b}
    end
    |> Enum.filter(fn {_a, _b, xx, yy} ->
      xx == tx and yy == ty
    end)
    |> Enum.map(fn {a, b, _xx, _yy} -> a * 3 + b * 1 end)
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")
Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

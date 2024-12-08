defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ":", trim: true))
    |> Enum.map(fn [h, str] ->
      nums =
        str
        |> String.split(" ", trim: true)
        |> Enum.map(&String.to_integer/1)

      {String.to_integer(h), nums}
    end)
  end

  def part_one(path) do
    path
    |> parse()
    |> Enum.filter(fn {sum, nums} ->
      check(nums, sum)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  def check([h | tail], sum) do
    do_check(tail, sum, h)
  end

  def do_check([], sum, curr), do: sum == curr

  def do_check([h | tail], sum, curr) do
    do_check(tail, sum, curr * h) || do_check(tail, sum, curr + h)
  end

  def part_two(path) do
    path
    |> parse()
    |> Enum.filter(fn {sum, nums} ->
      check2(nums, sum)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  def check2([h | tail], sum) do
    do_check2(tail, sum, h)
  end

  def do_check2([], sum, curr), do: sum == curr

  def do_check2([h | tail], sum, curr) do
    connect = "#{curr}#{h}" |> String.to_integer()

    do_check2(tail, sum, curr * h) ||
      do_check2(tail, sum, curr + h) ||
      do_check2(tail, sum, connect)
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

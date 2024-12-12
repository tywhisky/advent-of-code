defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.replace("\n", "")
    |> String.split(" ", trim: true)
  end

  def part_one(path) do
    origin = parse(path) |> Enum.frequencies()

    Stream.iterate(origin, &do_round(&1))
    |> Enum.at(75)
    |> Map.values()
    |> Enum.sum()
  end

  def do_round(map) do
    map
    |> Map.to_list()
    |> Enum.reduce(%{}, fn a, acc ->
      change(a, acc)
    end)
  end

  def change({"0", count}, map) do
    Map.update(map, "1", count, & &1 + count)
  end

  def change({string, count}, map) do
    len = String.length(string)
    mid = div(len, 2)

    if rem(len, 2) == 0 do
      left = String.slice(string, 0..(mid - 1)//1) |> remove_zero()
      right = String.slice(string, mid..-1//1) |> remove_zero()
      [{left, count}, {right, count}]
      
      map
      |> Map.update(left, count, & &1 + count)
      |> Map.update(right, count, & &1 + count)
    else
      new_num = (String.to_integer(string) * 2024) |> Integer.to_string()

      map
      |> Map.update(new_num, count, & &1 + count)
    end
  end

  defp remove_zero(string) do
    case String.replace(string, ~r/^0+/, "") do
      "" -> "0"
      result -> result
    end
  end
end

# Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")
# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

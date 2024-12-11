defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.replace("\n", "")
    |> String.split(" ", trim: true)
  end

  def part_one(path) do
    origin = parse(path)
    origin_map = Map.new(origin, fn x -> {x, 1} end)
    |> IO.inspect()

    Enum.reduce(1..25, origin_map, fn _, acc ->
      origin = Map.to_list(acc)
      round(origin, acc)
    end)
    |> Map.values()
    |> Enum.max()
  end
  
  def round([], result), do: result

  def round([_ | tail], result) do
    new_result =
      result
      |> Map.to_list()
      |> Enum.reduce(result, fn
        {"0", k}, acc ->
          Map.update(acc, "0", 0, &(&1 + 1))

        {v, k}, acc ->
          len = String.length(v)
          mid = div(len, 2)

          if rem(len, 2) == 0 do
            left = String.slice(v, 0..(mid - 1)//1) |> remove_zero()
            right = String.slice(v, mid..-1//1) |> remove_zero()

            acc
            |> Map.update(left, 0, &(&1 + 1))
            |> Map.update(right, 0, &(&1 + 1))
          else
            a =
              (String.to_integer(v) * 2024)
              |> Integer.to_string()

            Map.update(acc, a, 0, &(&1 + 1))
          end
      end)

    round(tail, new_result)
  end

  defp remove_zero(string) do
    case String.replace(string, ~r/^0+/, "") do
      "" -> "0"
      result -> result
    end
  end
end

 Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
# Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")
# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

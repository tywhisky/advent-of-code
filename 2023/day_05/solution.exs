defmodule Day05 do
  def part_one() do
    [seeds | maps] = parse()

    seeds
    |> Enum.map(&find_location(maps, &1))
    |> Enum.min()
  end

  defp find_location([], location), do: location

  defp find_location([section | tail], seed) do
    case(Enum.find(section, fn {{s, e}, _destination} -> seed >= s and seed <= e end)) do
      nil ->
        find_location(tail, seed)

      {{ss, _se}, distination} ->
        find_location(tail, seed - ss + distination)
    end
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> then(fn [[h] | tail] ->
      [_, h_nums] = String.split(h, ":")

      h_nums =
        h_nums
        |> String.split(" ", trim: true)
        |> Enum.map(&String.to_integer/1)

      tail =
        tail
        |> Enum.map(&tl/1)
        |> Enum.map(&Enum.map(&1, fn x -> String.split(x, " ", trim: true) end))
        |> Enum.map(&Enum.map(&1, fn x -> Enum.map(x, fn y -> String.to_integer(y) end) end))
        |> Enum.map(fn list ->
          Enum.reduce(list, [], fn [destination, source, length], acc ->
            {{source, source + length - 1}, destination}
            |> then(fn list -> [list | acc] |> Enum.reverse() end)
          end)
        end)

      [h_nums | tail]
    end)
  end
end

IO.inspect("The result of Day05 part one is: #{Day05.part_one()}")

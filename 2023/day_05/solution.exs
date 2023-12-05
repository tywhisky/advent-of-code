defmodule Day05 do
  def part_one() do
    [seeds | maps] = parse()

    seeds
    |> Enum.map(&find_location(maps, &1))
    |> Enum.min()
  end

  def part_two() do
    [seeds | maps] = parse()

    seeds
    |> Enum.chunk_every(2)
    |> Enum.map(fn [s, l] ->
      find_range(maps, [{s, s + l - 1}])
    end)
    |> List.flatten()
    |> Enum.reject(&(elem(&1, 0) == 0))
    |> Enum.min_by(&elem(&1, 0))
    |> elem(0)
  end

  defp find_range([], range), do: range

  defp find_range([section | tail], range) do
    new_range =
      range
      |> Enum.reduce([], fn {seed_start, seed_end}, acc ->
        acc1 =
          case Enum.find(section, fn {{s, e}, ds} ->
                 seed_start >= s and seed_start <= e
               end) do
            nil ->
              [{seed_start, seed_end} | acc]

            {{ss, ee}, ds} when ee >= seed_end ->
              [{ds - ss + seed_start, ds - ss + seed_end} | acc]

            {{ss, ee}, ds} ->
              [{ds - ss + seed_start, ee}, {ee + 1, seed_end} | acc]
          end

        acc2 =
          case Enum.find(section, fn {{s, e}, ds} -> seed_end >= s and seed_end <= e end) do
            nil ->
              [{seed_start, seed_end} | acc1]

            {{ss, ee}, ds} when ss <= seed_start ->
              [{ds - ss + seed_start, ds - ss + seed_end} | acc1]

            {{ss, ee}, ds} ->
              [{seed_start, ss - 1}, {ds - ss + ss, ds - ss + seed_end} | acc1]
          end

        acc3 =
          case Enum.find(section, fn {{s, e}, ds} -> seed_start < s and seed_end > e end) do
            nil ->
              acc2

            {{ss, ee}, ds} ->
              [{seed_start, ss - 1}, {ds - ss + ss, ds - ss + ee}, {ee + 1, seed_end} | acc2]
          end

        Enum.uniq(acc3)
      end)

    find_range(tail, new_range)
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
IO.inspect("The result of Day05 part two is: #{Day05.part_two()}")

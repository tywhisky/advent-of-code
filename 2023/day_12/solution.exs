defmodule Day12 do
  def part_one() do
    parse()
    |> Enum.map(fn {record, info} ->
      all =
        find_all(record, [])
        |> Stream.map(&Enum.join/1)
        |> Stream.map(&String.split(&1, ".", trim: true))
        |> Stream.map(&Enum.map(&1, fn s -> String.length(s) end))
        |> Stream.filter(&(&1 == info))
        |> length()
    end)
    |> Enum.sum()
  end

  def part_two() do
    parse()
    |> Enum.map(fn {l, i} ->
      new_l =
        List.duplicate(l, 5)
        |> Enum.join("?")
        |> String.graphemes()

      {new_l, List.duplicate(i, 5) |> List.flatten()}
    end)
    |> Enum.map(fn {record, info} ->
      all =
        find_all(record, [])
        |> Stream.map(&Enum.join/1)
        |> Stream.map(&String.split(&1, ".", trim: true))
        |> Stream.map(&Enum.map(&1, fn s -> String.length(s) end))
        |> Enum.filter(&(&1 == info))
        |> length()
    end)
    |> Enum.sum()
  end

  def find_all([], rest), do: [Enum.reverse(rest)]

  def find_all(["?" | tail], rest) do
    find_all(tail, ["." | rest]) ++ find_all(tail, ["#" | rest])
  end

  def find_all([h | tail], rest) do
    find_all(tail, [h | rest])
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " "))
    |> Enum.map(fn [str, cor] ->
      {String.graphemes(str),
       String.split(cor, ",", trim: true) |> Enum.map(&String.to_integer/1)}
    end)
  end
end

IO.puts("The Result of Day12 Part One is: #{Day12.part_one()}")
# IO.puts("The Result of Day12 Part Two is: #{Day12.part_two()}")

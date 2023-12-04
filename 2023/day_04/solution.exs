defmodule Day04 do
  def part_one() do
    input()
    |> Enum.reject(&(&1 == 0))
    |> Enum.map(&Integer.pow(2, &1 - 1))
    |> Enum.sum()
  end

  def part_two() do
    cards =
      input()
      |> Enum.with_index(1)
      |> Enum.map(fn {a, b} -> {b, a} end)

    recursive(Enum.to_list(length(cards)..1), Map.new(cards))
  end

  defp recursive([], map) do
    map
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  defp recursive([idx | tail], map) do
    if map[idx] == 0 do
      recursive(tail, Map.put(map, idx, 1))
    else
      sum =
        (idx + 1)..(idx + map[idx])
        |> Enum.reduce(0, fn i, acc -> acc + map[i] end)

      recursive(tail, Map.put(map, idx, sum + 1))
    end
  end

  def input() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ":"))
    |> Enum.map(&tl/1)
    |> Enum.map(fn [str] -> String.split(str, "|") end)
    |> Enum.map(fn [win, own] ->
      [String.split(win, " ", trim: true), String.split(own, " ", trim: true)]
    end)
    |> Enum.map(fn [win_list, own_list] ->
      (win_list -- win_list -- own_list)
      |> length()
    end)
  end
end

IO.puts("The Result of Day04 Part One is: #{Day04.part_one()}")
IO.puts("The Result of Day04 Part Two is: #{Day04.part_two()}")

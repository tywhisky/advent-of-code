defmodule Day01 do
  @word_to_number %{
    "one" => 1,
    "two" => 2,
    "three" => 3,
    "four" => 4,
    "five" => 5,
    "six" => 6,
    "seven" => 7,
    "eight" => 8,
    "nine" => 9,
    "0" => 0,
    "1" => 1,
    "2" => 2,
    "3" => 3,
    "4" => 4,
    "5" => 5,
    "6" => 6,
    "7" => 7,
    "8" => 8,
    "9" => 9
  }

  @rgx Map.keys(@word_to_number) |> Enum.join("|")

  def part_one() do
    input()
    |> Enum.map(&Regex.scan(~r/[0-9]/, &1))
    |> common()
  end

  def part_two() do
    input()
    |> Enum.map(&recursive(String.graphemes(&1), []))
    |> common()
  end

  defp recursive([], result), do: Enum.reverse(result)

  defp recursive([_ | tail] = list, acc) do
    str = Enum.join(list, "")

    case Regex.scan(~r/#{@rgx}/, str) do
      [h | _] -> recursive(tail, [h | acc])
      [] -> recursive([], acc)
    end
  end

  defp common(list) do
    list
    |> Enum.map(&List.flatten/1)
    |> Enum.map(fn l ->
      @word_to_number[List.first(l)] * 10 + @word_to_number[List.last(l)]
    end)
    |> Enum.sum()
  end

  def input() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
  end
end

IO.puts("The part one's solution: #{Day01.part_one()}")
IO.puts("The part two's solution: #{Day01.part_two()}")

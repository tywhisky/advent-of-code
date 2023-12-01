defmodule Day01 do
  @word_to_number %{
    "one" => "1",
    "two" => "2",
    "three" => "3",
    "four" => "4",
    "five" => "5",
    "six" => "6",
    "seven" => "7",
    "eight" => "8",
    "nine" => "9"
  }

  @reverse_word_to_number Enum.map(@word_to_number, fn {k, v} -> {String.reverse(k), v} end)
                          |> Map.new()

  def part_one() do
    input()
    |> common()
  end

  def part_two() do
    num_words = %{
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
    input()
    |> Enum.map(fn str ->
      recursive(String.graphemes(str), [])
    end)
    |> Enum.map(fn x ->
      [f] = List.first(x)
      [l] = List.last(x)

      "#{num_words[f]}#{num_words[l]}"
    end)
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
    |> dbg()
  end

  defp recursive([], result), do: Enum.reverse(result)

  defp recursive([_ | tail] = list, acc) do
    str = Enum.join(list, "")

    case Regex.scan(~r/one|two|three|four|five|six|seven|eight|nine|1|2|3|4|5|6|7|8|9/, str) do
      [h | _] -> recursive(tail, [h | acc])
      [] -> recursive([], acc)
    end
  end

  defp common(list) do
    list
    |> Enum.map(&String.replace(&1, ~r/[^0-9]/, ""))
    |> Enum.map(&"#{String.at(&1, 0)}#{String.at(&1, -1)}")
    |> Enum.map(&String.to_integer/1)
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

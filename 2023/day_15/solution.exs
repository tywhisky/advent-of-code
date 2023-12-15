defmodule Day15 do
  def part_one() do
    parse()
    |> Enum.map(fn char_list ->
      Enum.reduce(char_list, 0, fn c, acc ->
        ((c + acc) * 17)
        |> rem(256)
      end)
    end)
    |> Enum.sum()
    |> dbg()
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.replace("\n", "")
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end
end

IO.puts("The Result of Day15 Part One is: #{Day15.part_one()}")
# IO.puts("The Result of Day15 Part Two is: #{Day15.part_two()}")

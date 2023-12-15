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
  end

  def part_two() do
    parse()
    |> Enum.reduce([], fn char_list, acc ->
      if ?= in char_list do
        {_, list} = Enum.split_with(char_list, &(&1 == ?=))
        [num | list] = Enum.reverse(list)
        [{Enum.reverse(list), num - 48} | acc]
      else
        {_, list} = Enum.split_with(char_list, &(&1 == ?-))
        tar = Enum.find(acc, &(elem(&1, 0) == list))
        acc -- [tar]
      end
    end)
    |> Enum.reverse()
    |> Enum.reduce([], fn
      {_char_list, num}, [] ->
        [num * num]

      {_char_list, num}, [h | _] = acc ->
        [h * num | acc]
    end)
    |> Enum.sum()
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> String.replace("\n", "")
    |> String.split(",", trim: true)
    |> Enum.map(&String.to_charlist/1)
  end
end

IO.puts("The Result of Day15 Part One is: #{Day15.part_one()}")
IO.puts("The Result of Day15 Part Two is: #{Day15.part_two()}")

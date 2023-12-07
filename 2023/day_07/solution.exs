defmodule Day07 do
  @alpha_order ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]
               |> Enum.with_index(1)
               |> Map.new()

  def part_one() do
    parse()
    |> Enum.sort(fn {freq1, list1, _bid1}, {freq2, list2, _bid2} ->
      f1 = Map.to_list(freq1)
      f2 = Map.to_list(freq2)

      {_, pair1} = Enum.max_by(f1, &elem(&1, 1))
      {_, pair2} = Enum.max_by(f2, &elem(&1, 1))

      len1 = length(f1)
      len2 = length(f2)

      if len1 == len2 do
        if pair1 == pair2 do
          compare_first_char(list1, list2)
        else
          pair1 < pair2
        end
      else
        len1 > len2
      end
    end)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, _, bid}, idx} -> bid * idx  end)
    |> Enum.sum()
  end

  def compare_first_char([c1 | tail1], [c2 | tail2]) when c1 == c2 do
    compare_first_char(tail1, tail2)
  end

  def compare_first_char([c1 | _], [c2 | _]) do
    @alpha_order[c1] > @alpha_order[c2]
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(fn [card, bid] ->
      card_list = String.graphemes(card)

      {Enum.frequencies(card_list), card_list, String.to_integer(bid)}
    end)
  end
end

IO.inspect("The result of Day07 part one is: #{Day07.part_one()}")

defmodule Day07 do
  def part_one() do
    order =
      ["A", "K", "Q", "J", "T", "9", "8", "7", "6", "5", "4", "3", "2"]
      |> Enum.with_index(1)
      |> Map.new()

    sort_and_sum(parse(), order)
  end

  def part_two() do
    order =
      ["A", "K", "Q", "T", "9", "8", "7", "6", "5", "4", "3", "2", "J"]
      |> Enum.with_index(1)
      |> Map.new()

    parse()
    |> Enum.map(fn {freq, list, bid} ->
      f = Map.to_list(freq)

      case freq["J"] do
        nil ->
          {freq, list, bid}

        5 ->
          {freq, list, bid}

        qty ->
          {max_chat, max_count} =
            f
            |> Enum.reject(&(elem(&1, 0) == "J"))
            |> Enum.max_by(&elem(&1, 1))

          new_freq =
            freq
            |> Map.delete("J")
            |> Map.put(max_chat, qty + max_count)

          {new_freq, list, bid}
      end
    end)
    |> sort_and_sum(order)
  end

  def sort_and_sum(data, order) do
    data
    |> Enum.sort(fn {freq1, list1, _bid1}, {freq2, list2, _bid2} ->
      f1 = Map.to_list(freq1)
      f2 = Map.to_list(freq2)

      {_, pair1} = Enum.max_by(f1, &elem(&1, 1))
      {_, pair2} = Enum.max_by(f2, &elem(&1, 1))

      len1 = length(f1)
      len2 = length(f2)

      if len1 == len2 do
        if pair1 == pair2 do
          compare_first_char(list1, list2, order)
        else
          pair1 < pair2
        end
      else
        len1 > len2
      end
    end)
    |> Enum.with_index(1)
    |> Enum.map(fn {{_, _, bid}, idx} -> bid * idx end)
    |> Enum.sum()
  end

  def compare_first_char([], [], _order), do: false

  def compare_first_char([c1 | tail1], [c2 | tail2], order) when c1 == c2 do
    compare_first_char(tail1, tail2, order)
  end

  def compare_first_char([c1 | _], [c2 | _], order) do
    order[c1] > order[c2]
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
IO.inspect("The result of Day07 part two is: #{Day07.part_two()}")

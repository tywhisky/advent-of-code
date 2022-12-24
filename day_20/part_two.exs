defmodule Day20.PartTwo do
  def find_result(list, len) do
    zero_index = Enum.find_index(list, &(elem(&1, 1) == 0))

    [rem(zero_index + 1000, len), rem(zero_index + 2000, len), rem(zero_index + 3000, len)]
    |> Enum.map(&Enum.at(list, &1))
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def run(list) do
    list_with_index =
      list
      |> Enum.map(&(&1 * 811_589_153))
      |> Enum.with_index(fn value, index -> {index, value} end)

    len = length(list_with_index)
    indexs = 0..(len - 1)

    Enum.reduce(1..10, list_with_index, fn _, acc ->
      Enum.reduce(indexs, acc, fn id, inner_acc ->
        index = Enum.find_index(inner_acc, fn {i, _} -> i == id end)
        {_, val} = Enum.at(inner_acc, index)
        step = Integer.mod(val, len - 1)
        offset = if index + step >= len, do: 1, else: 0
        new_index = Integer.mod(index + step, len) + offset

        inner_acc
        |> List.delete_at(index)
        |> List.insert_at(new_index, {id, val})
      end)
    end)
    |> find_result(len)
  end
end

list =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.to_integer/1)

Day20.PartTwo.run(list)
|> IO.inspect()

defmodule Day20.PartOne do
  def find_result(list, len) do
    zero_index = Enum.find_index(list, &(elem(&1, 1) == 0))

    [rem(zero_index + 1000, len), rem(zero_index + 2000, len), rem(zero_index + 3000, len)]
    |> Enum.map(&Enum.at(list, &1))
    |> Enum.map(&elem(&1, 1))
    |> Enum.sum()
  end

  def run(list) do
    list_with_index = Enum.with_index(list, fn value, index -> {index, value} end)

    len = length(list_with_index)
    indexs = 0..(len - 1)

    Enum.reduce(indexs, list_with_index, fn id, acc ->
      index = Enum.find_index(acc, fn {i, _} -> i == id end)
      {_, val} = Enum.at(acc, index)
      step = Integer.mod(val, len - 1)
      offset = if index + step >= len, do: 1, else: 0
      new_index = Integer.mod(index + step, len) + offset

      acc
      |> List.delete_at(index)
      |> List.insert_at(new_index, {id, val})
    end)
    |> find_result(len)
  end
end

list =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.to_integer/1)

Day20.PartOne.run(list)
|> IO.inspect()

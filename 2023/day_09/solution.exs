defmodule Day09 do
  def part_one() do
    parse()
    |> Enum.map(&build_steps(&1, [Enum.reverse(&1)]))
    |> Enum.map(fn list ->
      Enum.reduce(list, 0, fn [h | _], acc ->
        h + acc
      end)
    end)
    |> Enum.sum()
  end

  def part_two() do
    parse()
    |> Enum.map(&build_steps(&1, [Enum.reverse(&1)]))
    |> Enum.map(&Enum.map(&1, fn x -> Enum.reverse(x) end))
    |> Enum.map(fn list ->
      Enum.reduce(list, {0, 0}, fn [h | _], {curr, acc} ->
        {h - curr, h - curr + acc}
      end)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
  end

  def build_steps(list, result) do
    next =
      list
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.map(fn [p, n] -> n - p end)

    case Enum.all?(next, &(&1 == 0)) do
      true -> [Enum.reverse(next) | result]
      _ -> build_steps(next, [Enum.reverse(next) | result])
    end
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(&Enum.map(&1, fn s -> String.to_integer(s) end))
  end
end

IO.puts("The part one's solution: #{Day09.part_one()}")
IO.puts("The part two's solution: #{Day09.part_two()}")

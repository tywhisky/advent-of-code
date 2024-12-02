defmodule Solution do
  def input() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(&Enum.map(&1, fn x -> String.to_integer(x) end))
  end

  def solution_one() do
    input()
    |> Enum.map(fn row ->
      Enum.chunk_every(row, 2, 1, :discard)
      |> Enum.map(fn [a, b] -> a - b end)
    end)
    |> Enum.filter(fn row ->
      Enum.all?(row, &(abs(&1) <= 3 and abs(&1) >= -3)) and
        (Enum.all?(row, &(&1 > 0)) or
           Enum.all?(row, &(&1 < 0)))
    end)
    |> Enum.count()

    # 516
  end

  def solution_two() do
    input()
    |> Enum.map(fn row ->
      Enum.chunk_every(row, 2, 1, :discard)
      |> Enum.map(fn [a, b] -> a - b end)
    end)
    |> Enum.reject(fn row ->
      Enum.all?(row, &(abs(&1) <= 3 and abs(&1) >= -3)) and
        (Enum.all?(row, &(&1 > 0)) or
           Enum.all?(row, &(&1 < 0)))
    end)
    |> Enum.map(fn row ->
      expand_row(row, [], [])
      |> Enum.map(fn row ->
        Enum.chunk_every(row, 2, 1, :discard)
        |> Enum.map(fn [a, b] -> a - b end)
      end)
      |> Enum.any?(fn row ->
        Enum.all?(row, &(abs(&1) <= 3 and abs(&1) >= -3)) and
          (Enum.all?(row, &(&1 > 0)) or
             Enum.all?(row, &(&1 < 0)))
      end)
    end)
    |> Enum.count(& &1 == true)
  end

  def expand_row([], _rest, result), do: result

  def expand_row([h | tail], rest, result) do
    expand_row(tail, [h | rest], [Enum.reverse(rest) ++ tail | result])
  end
end

Solution.solution_two()
|> dbg()

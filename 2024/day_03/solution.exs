defmodule Solution do
  def parser() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
  end

  def part_two() do
    parser()
    |> Enum.map(&match/1)
    |> Enum.map(fn row ->
      row
      |> Enum.reduce({[true], 0}, fn
        ["do()"], {stack, sum} ->
          {[true | stack], sum}

        ["don't()"], {stack, sum} ->
          {[false | stack], sum}

        [_, _a, _b], {[], sum} ->
          {[], sum}

        [_, a, b], {[true | stack], sum} ->
          {stack, String.to_integer(a) * String.to_integer(b) + sum}

        [_, _a, _b], {[false | stack], sum} ->
          {stack, sum}
      end)
      |> elem(1)
    end)
    |> Enum.sum()
  end

  def match(row) do
    regex = ~r/do\(\)|don't\(\)|mul\((\d{1,3}),(\d{1,3})\)/
    Regex.scan(regex, row)
  end
end

Solution.part_two()
|> IO.inspect()

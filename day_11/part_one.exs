defmodule Day11.PartOne do
  def change("old"), do: :old
  def change(num_str), do: String.to_integer(num_str)

  def run(monkeys, 20), do: monkeys

  def run(monkeys, round) do
    new_monkeys =
      monkeys
      |> Enum.reduce(monkeys, fn {idx, monkey}, result ->
        curr_start = get_in(result, [idx, :start])

        curr_start
        |> Enum.map(&calculate(&1, monkey.operation))
        |> Enum.reduce(result, fn new, acc ->
          case rem(new, monkey.div) do
            0 ->
              update_in(acc, [monkey.true_branch, :start], &(&1 ++ [new]))
              |> update_in([idx, :times], &(&1 + 1))

            _ ->
              update_in(acc, [monkey.false_branch, :start], &(&1 ++ [new]))
              |> update_in([idx, :times], &(&1 + 1))
          end
        end)
        |> put_in([idx, :start], [])
      end)

    run(new_monkeys, round + 1)
  end

  def calculate(old, operation) do
    operation
    |> Enum.map(fn
      :old -> old
      num_or_op -> num_or_op
    end)
    |> do_cal()
  end

  def do_cal([num_1, "*", num_2]), do: div(num_1 * num_2, 3)
  def do_cal([num_1, "+", num_2]), do: div(num_1 + num_2, 3)
end

regex = ~r/[A-Za-z_: \n]/

File.read!("#{__DIR__}/input.txt")
|> String.split("\n\n")
|> Enum.map(&String.split(&1, "\n "))
|> Enum.map(fn [
                 idx_str,
                 start_str,
                 operation_str,
                 div_str,
                 true_branch_str,
                 false_branch_str
               ] ->
  idx =
    idx_str
    |> String.replace(regex, "")
    |> String.to_integer()

  start =
    start_str
    |> String.replace(regex, "")
    |> String.split(",")
    |> Enum.map(&String.to_integer(&1))

  [prefix, op, suffix] =
    operation_str
    |> String.replace(" Operation: new = ", "")
    |> String.split(" ")

  div =
    div_str
    |> String.replace(regex, "")
    |> String.to_integer()

  true_branch =
    true_branch_str
    |> String.replace(regex, "")
    |> String.to_integer()

  false_branch =
    false_branch_str
    |> String.replace(regex, "")
    |> String.to_integer()

  {idx,
   %{
     start: start,
     operation: [Day11.PartOne.change(prefix), op, Day11.PartOne.change(suffix)],
     div: div,
     true_branch: true_branch,
     false_branch: false_branch,
     times: 0
   }}
end)
|> Map.new()
|> Day11.PartOne.run(0)
|> Enum.map(&elem(&1, 1).times)
|> Enum.sort(:desc)
|> Enum.take(2)
|> Enum.product()
|> IO.inspect()

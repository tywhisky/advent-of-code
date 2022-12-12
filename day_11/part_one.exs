defmodule Day11.PartOne do
  def change("old"), do: :old
  def change(num_str), do: String.to_integer(num_str)

  def run(monkeys, 2), do: monkeys

  def run(monkeys, round) do
    new_monkeys =
      monkeys
      |> Enum.reduce(monkeys, fn {idx, monkey}, result ->
        updated =
          monkey.start
          |> Enum.map(&calculate(&1, monkey.operation))
          |> IO.inspect(charlists: :as_lists)
          |> Enum.reduce(result, fn new, acc ->
            case rem(new, monkey.div) do
              0 ->
                put_in(
                  acc,
                  [monkey.true_branch, :start],
                  get_in(acc, [monkey.true_branch, :start]) ++
                    [div(new, monkey.div)]
                )

              _ ->
                put_in(
                  acc,
                  [monkey.false_branch, :start],
                  get_in(acc, [monkey.false_branch, :start]) ++
                    [div(new, monkey.div)]
                )
            end
          end)

        put_in(updated, [idx, :start], [])
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

  def do_cal([num_1, "*", num_2]), do: num_1 * num_2
  def do_cal([num_1, "+", num_2]), do: num_1 + num_2
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
     false_branch: false_branch
   }}
end)
|> Map.new()
|> Day11.PartOne.run(1)
|> IO.inspect()

defmodule Day19 do
  def part_one() do
    {workflows, ratings} = parse()

    do_workflow(ratings, "in", workflows, 0)
  end

  def do_workflow([], _, _, result), do: result

  def do_workflow([rating | tail], curr, workflows, result) do
    case do_compute(rating, workflows[curr]) do
      "A" ->
        sum = Enum.map(rating, &elem(&1, 1)) |> Enum.sum()
        do_workflow(tail, "in", workflows, sum + result)

      "R" ->
        do_workflow(tail, "in", workflows, result)

      next ->
        do_workflow([rating | tail], next, workflows, result)
    end
  end

  def do_compute(_rating, [[final]]), do: final

  def do_compute(rating, [[condition, next] | tail]) do
    case Code.eval_string(condition, rating) do
      {true, _} -> next
      {false, _} -> do_compute(rating, tail)
    end
  end

  def parse() do
    [workflows, ratings] =
      "input.txt"
      |> File.read!()
      |> String.split("\n\n", trim: true)

    workflows =
      workflows
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, ~r/[{}]/, trim: true))
      |> Enum.map(fn [name, conditions] ->
        conditions =
          conditions
          |> String.split(~r/[,]/)
          |> Enum.map(&String.split(&1, ":", trim: true))

        {name, conditions}
      end)
      |> Map.new()

    ratings =
      ratings
      |> String.replace(~r/[{}]/, "")
      |> String.split("\n", trim: true)
      |> Enum.map(fn str ->
        str
        |> String.split(",", trim: true)
        |> Enum.map(&String.split(&1, "="))
        |> Keyword.new(fn [key, value] ->
          {String.to_existing_atom(key), String.to_integer(value)}
        end)
      end)

    {workflows, ratings}
  end
end

IO.inspect(Day19.part_one())

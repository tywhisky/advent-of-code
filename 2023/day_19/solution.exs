defmodule Day19 do
  def part_one() do
    {workflows, ratings} = parse()

    do_workflow(ratings, "in", workflows, 0)
  end

  def part_two() do
    {workflows, _ratings} = parse()

    collect_conditions(workflows["in"], workflows, [], 0)
  end

  def collect_conditions([["A"]], _workflow, path, result), do: intersection(path) + result
  def collect_conditions([["R"]], _, _, result), do: result

  def collect_conditions([[next]], workflow, path, result),
    do: collect_conditions(workflow[next], workflow, path, result)

  def collect_conditions([[condition, "A"] | tail], workflow, path, result) do
    new_result = intersection([condition | path]) + result

    oppo_condition =
      condition
      |> String.match?(~r/>/)
      |> case do
        true -> String.replace(condition, ">", "S")
        false -> String.replace(condition, "<", "B")
      end

    collect_conditions(tail, workflow, [oppo_condition | path], new_result)
  end

  def collect_conditions([[condition, "R"] | tail], workflow, path, result) do
    oppo_condition =
      condition
      |> String.match?(~r/>/)
      |> case do
        true -> String.replace(condition, ">", "S")
        false -> String.replace(condition, "<", "B")
      end

    collect_conditions(tail, workflow, [oppo_condition | path], result)
  end

  def collect_conditions([[condition, next] | tail], workflow, path, result) do
    oppo_condition =
      condition
      |> String.match?(~r/>/)
      |> case do
        true -> String.replace(condition, ">", "S")
        false -> String.replace(condition, "<", "B")
      end

    new_result = collect_conditions(workflow[next], workflow, [condition | path], result)
    collect_conditions(tail, workflow, [oppo_condition | path], new_result)
  end

  def intersection(path) do
    total = %{
      "x" => MapSet.new(1..4000),
      "a" => MapSet.new(1..4000),
      "s" => MapSet.new(1..4000),
      "m" => MapSet.new(1..4000)
    }

    path
    |> Enum.map(&String.graphemes/1)
    |> Enum.reduce(total, fn [rating, op | tail], acc ->
      num = Enum.join(tail) |> String.to_integer()

      range =
        case op do
          "<" -> 1..(num - 1)
          ">" -> (num + 1)..4000
          "S" -> 1..num
          "B" -> num..4000
        end

      new_set =
        range
        |> MapSet.new()
        |> MapSet.intersection(acc[rating])

      Map.put(acc, rating, new_set)
    end)
    |> Enum.map(fn {_, set} -> MapSet.size(set) end)
    |> Enum.product()
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
IO.inspect(Day19.part_two())

defmodule Day08 do
  def part_one() do
    {actions, map} = parse()
    recursive(actions, "AAA", actions, map, 0)
  end

  def recursive([], current, actions_backup, map, count) do
    recursive(actions_backup, current, actions_backup, map, count)
  end

  def recursive([h | tail], current, actions_backup, map, count) do
    right_or_left = if h == "L", do: 0, else: 1

    case map[current] |> elem(right_or_left) do
      "ZZZ" -> count + 1
      next -> recursive(tail, next, actions_backup, map, count + 1)
    end
  end

  def parse() do
    [actions | nodes] =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    map =
      nodes
      |> Enum.map(&String.replace(&1, ~r/[()=,]/, ""))
      |> Enum.map(&String.split(&1, " ", trim: true))
      |> Map.new(fn [start, left, right] -> {start, {left, right}} end)

    {String.graphemes(actions), map}
  end
end

IO.inspect("The result of Day08 Part One is: #{Day08.part_one()}")

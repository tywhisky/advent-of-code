defmodule Day17.PartTwo do
  def first_rock(height) do
    [{height, 2}, {height, 3}, {height, 4}, {height, 5}]
  end

  def second_rock(height) do
    [{height, 3}, {height + 1, 2}, {height + 1, 3}, {height + 1, 4}, {height + 2, 3}]
  end

  def third_rock(height) do
    [
      {height + 2, 4},
      {height + 1, 4},
      {height, 4},
      {height, 3},
      {height, 2}
    ]
  end

  def forth_rock(height) do
    [{height, 2}, {height + 1, 2}, {height + 2, 2}, {height + 3, 2}]
  end

  def fifth_rock(height) do
    [{height, 2}, {height, 3}, {height + 1, 2}, {height + 1, 3}]
  end

  def build_rock(1, height), do: first_rock(height)
  def build_rock(2, height), do: second_rock(height)
  def build_rock(3, height), do: third_rock(height)
  def build_rock(4, height), do: forth_rock(height)
  def build_rock(0, height), do: fifth_rock(height)

  def run(_gas, 30, record), do: record

  def run([h | _] = gas, count, record) do
    {highest, _} = Enum.max_by(record, &elem(&1, 0))
    rock = build_rock(rem(count, 5), highest + 4)
    {new_record, tail_gas} = fall(gas, rock, record)

    {new_highest, _} = Enum.max_by(new_record, &elem(&1, 0))
    IO.inspect({h, new_highest - highest})

    run(tail_gas, count + 1, new_record)
  end

  def fall([direction | tail], rock, record) do
    move_rock =
      case direction do
        "<" -> Enum.map(rock, fn {h, x} -> {h, x - 1} end)
        ">" -> Enum.map(rock, fn {h, x} -> {h, x + 1} end)
      end

    rock =
      if MapSet.disjoint?(MapSet.new(move_rock), record) &&
           Enum.all?(move_rock, &(elem(&1, 1) >= 0 and elem(&1, 1) <= 6)) do
        move_rock
      else
        rock
      end

    down_rock = Enum.map(rock, fn {h, x} -> {h - 1, x} end)

    if MapSet.disjoint?(MapSet.new(down_rock), record) do
      fall(tail, down_rock, record)
    else
    new_record =
      (rock ++ MapSet.to_list(record))
      |> Enum.group_by(&elem(&1, 0))
      |> Enum.reject(fn {_, v} -> length(v) == 7 end)
      |> Enum.flat_map(&elem(&1, 1))
      |> MapSet.new()

      {new_record, tail}
    end
  end
end

init = MapSet.new([{0, 0}, {0, 1}, {0, 2}, {0, 3}, {0, 4}, {0, 5}, {0, 6}])

File.read!("#{__DIR__}/input.txt")
|> String.graphemes()
|> List.duplicate(999)
|> List.flatten()
|> Day17.PartTwo.run(1, init)
|> Enum.max_by(&elem(&1, 0))
|> IO.inspect()

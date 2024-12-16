defmodule Solution do
  def parse(path) do
    [coor, actions] =
      path
      |> File.read!()
      |> String.split("\n\n", trim: true)

    coor =
      coor
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, col_idx} ->
        row
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {c, row_idx} ->
          {{col_idx, row_idx}, c}
        end)
      end)

    actions =
      actions
      |> String.replace("\n", "")
      |> String.graphemes()

    {start, _} = Enum.find(coor, &(elem(&1, 1) == "@"))

    {start, Map.new(coor), actions}
  end

  def part_one(path) do
    {start, map, actions} = parse(path)

    move(actions, start, map)
    |> Map.to_list()
    #    |> debug()
    |> Enum.filter(&(elem(&1, 1) == "O"))
    |> Enum.map(fn {{a, b}, _} ->
      100 * a + b
    end)
    |> Enum.sum()
  end

  def debug(list) do
    list
    |> Enum.group_by(fn {{x, y}, c} -> x end)
    |> Enum.to_list()
    |> Enum.sort_by(&elem(&1, 0))
    |> Enum.map(&elem(&1, 1))
    |> Enum.map(&Enum.sort_by(&1, fn {{a, b}, _} -> b end))
    |> Enum.map(&Enum.map(&1, fn x -> elem(x, 1) end))
    |> Enum.map(&Enum.join(&1, ""))
    |> Enum.map(&IO.inspect/1)
  end

  def move([], _, map), do: map

  def move(["<" | tail], {x, y}, map) do
    directions =
      (y - 1)..0
      |> Enum.to_list()
      |> Enum.map(fn a -> {x, a} end)

    {wx, wy} = Enum.find(directions, &(map[&1] == "#"))

    directions
    |> Enum.find(&(map[&1] == "."))
    |> case do
      nil ->
        move(tail, {x, y}, map)

      {tx, ty} ->
        if wy > ty do
          move(tail, {x, y}, map)
        else
          others =
            (y - 1)..ty
            |> Enum.to_list()
            |> Map.new(fn ny -> {{x, ny}, "O"} end)

          robot = Map.new([{{x, y - 1}, "@"}, {{x, y}, "."}])

          new_map = Map.merge(map, Map.merge(others, robot))
          move(tail, {x, y - 1}, new_map)
        end
    end
  end

  def move(["^" | tail], {x, y}, map) do
    directions =
      (x - 1)..0
      |> Enum.to_list()
      |> Enum.map(fn a -> {a, y} end)

    {wx, wy} = Enum.find(directions, &(map[&1] == "#"))

    directions
    |> Enum.find(&(map[&1] == "."))
    |> case do
      nil ->
        move(tail, {x, y}, map)

      {tx, ty} ->
        if wx > tx do
          move(tail, {x - 1, y}, map)
        else
          others =
            (x - 1)..tx
            |> Enum.to_list()
            |> Map.new(fn nx -> {{nx, y}, "O"} end)

          robot = Map.new([{{x - 1, y}, "@"}, {{x, y}, "."}])

          new_map = Map.merge(map, Map.merge(others, robot))
          move(tail, {x - 1, y}, new_map)
        end
    end
  end

  def move(["v" | tail], {x, y}, map) do
    directions =
      (x + 1)..50
      |> Enum.to_list()
      |> Enum.map(fn a -> {a, y} end)

    {wx, wy} = Enum.find(directions, &(map[&1] == "#"))

    directions
    |> Enum.find(&(map[&1] == "."))
    |> case do
      nil ->
        move(tail, {x, y}, map)

      {tx, ty} ->
        if wx < tx do
          move(tail, {x, y}, map)
        else
          others =
            (x + 1)..tx
            |> Enum.to_list()
            |> Map.new(fn nx -> {{nx, y}, "O"} end)

          robot = Map.new([{{x + 1, y}, "@"}, {{x, y}, "."}])

          new_map = Map.merge(map, Map.merge(others, robot))
          move(tail, {x + 1, y}, new_map)
        end
    end
  end

  def move([">" | tail], {x, y}, map) do
    directions =
      (y + 1)..50
      |> Enum.to_list()
      |> Enum.map(fn a -> {x, a} end)

    {wx, wy} = Enum.find(directions, &(map[&1] == "#"))

    directions
    |> Enum.find(&(map[&1] == "."))
    |> case do
      nil ->
        move(tail, {x, y}, map)

      {tx, ty} ->
        if wy < ty do
          move(tail, {x, y}, map)
        else
          others =
            (y + 1)..ty
            |> Enum.to_list()
            |> Map.new(fn ny -> {{x, ny}, "O"} end)

          robot = Map.new([{{x, y + 1}, "@"}, {{x, y}, "."}])

          new_map = Map.merge(map, Map.merge(others, robot))
          move(tail, {x, y + 1}, new_map)
        end
    end
  end
end

Solution.part_one("test.txt") |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt") |> IO.inspect(label: "Part One with input.txt")

# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
# Solution.part_two("input.txt") |> IO.inspect(label: "Part Two with inpt.txt")

defmodule Day10 do
  def part_one() do
    list = parse()
    {start, _} = Enum.find(list, &(elem(&1, 1) == "S"))
    map = Map.new(list)

    bfs(get_starts(start, map), 0, map, %{})
  end

  def bfs(currs, value, map, record) do
    new_record = Enum.reduce(currs, record, &Map.put(&2, &1, true))

    currs
    |> Enum.flat_map(&find_next(map[&1], &1))
    |> Enum.reject(&is_nil(map[&1]))
    |> Enum.reject(&(map[&1] == "."))
    |> Enum.reject(&record[&1])
    |> case do
      [] -> value + 1
      nexts -> bfs(nexts, value + 1, map, new_record)
    end
  end

  def get_starts({col, row}, map) do
    [
      {{col + 1, row}, :down},
      {{col - 1, row}, :top},
      {{col, row + 1}, :right},
      {{col, row - 1}, :left}
    ]
    |> Enum.map(fn
      {p, :down} -> (map[p] in ["|", "J", "L"] && p) || nil
      {p, :top} -> (map[p] in ["|", "7", "F"] && p) || nil
      {p, :left} -> (map[p] in ["-", "F", "L"] && p) || nil
      {p, :right} -> (map[p] in ["-", "7", "J"] && p) || nil
    end)
    |> Enum.reject(&is_nil/1)
  end

  def find_next("-", {col, row}), do: [{col, row + 1}, {col, row - 1}]
  def find_next("7", {col, row}), do: [{col + 1, row}, {col, row - 1}]
  def find_next("|", {col, row}), do: [{col + 1, row}, {col - 1, row}]
  def find_next("J", {col, row}), do: [{col - 1, row}, {col, row - 1}]
  def find_next("L", {col, row}), do: [{col, row + 1}, {col - 1, row}]
  def find_next("F", {col, row}), do: [{col + 1, row}, {col, row + 1}]
  def find_next("S", {_col, _row}), do: []

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {str, col_idx} ->
      str
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {r, row_idx} -> {{col_idx, row_idx}, r} end)
    end)
  end
end

IO.puts("The Result of Day10 Part One is: #{Day10.part_one()}")
# IO.puts("The Result of Day10 Part Two is: #{Day10.part_two()}")

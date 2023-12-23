defmodule Day23 do
  def part_one() do
    {start, target, map} = parse()

    dfs(start, target, map, 0, %{})
  end

  def dfs(curr, target, _map, path, _record) when curr == target, do: path

  def dfs({row_idx, col_idx} = curr, target, map, path, record) do
    record = Map.put(record, curr, true)

    case map[curr] do
      ">" ->
        [{row_idx, col_idx + 1}]

      "<" ->
        [{row_idx, col_idx - 1}]

      "v" ->
        [{row_idx + 1, col_idx}]

      "#" ->
        []

      "." ->
        [
          {row_idx, col_idx + 1},
          {row_idx, col_idx - 1},
          {row_idx + 1, col_idx},
          {row_idx - 1, col_idx}
        ]
    end
    |> Enum.reject(&is_nil(map[&1]))
    |> Enum.reject(&record[&1])
    |> Enum.reduce(path, fn next, acc_result ->
      dfs(next, target, map, path + 1, record)
      |> max(acc_result)
    end)
  end

  def parse() do
    [line | _] =
      strs =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    list =
      strs
      |> Enum.with_index()
      |> Enum.flat_map(fn {str, row_idx} ->
        str
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {c, col_idx} -> {{row_idx, col_idx}, c} end)
      end)

    start = {0, 1}
    target = {length(strs) - 1, String.length(line) - 2}
    {start, target, Map.new(list)}
  end
end

Day23.part_one() |> IO.inspect()

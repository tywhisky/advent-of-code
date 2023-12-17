defmodule Day17 do
  def part_one() do
    {target, map} = parse()

    [{{0, 0}, :right, 0}, {{0, 0}, :down, 0}]
    |> dfs(map, %{{0, 0} => 0})
    |> Map.get(target)
  end

  def dfs([], _, record), do: record

  def dfs([{c, _, _} = curr | tail], map, record) do
    nexts =
      curr
      |> nexts()
      |> Enum.reject(fn {n, _, _} -> map[n] == nil end)
      |> Enum.reject(fn {n, _, _} ->
        record[n] < record[c] + map[n]
      end)

    new_record =
      nexts
      |> Enum.reduce(record, fn {next, _, _}, acc -> Map.put(acc, next, record[c] + map[next]) end)

    dfs(nexts ++ tail, map, new_record)
  end

  def nexts({curr, dir, 3}) when dir in [:left, :right] do
    [
      {to_top(curr), :top, 1},
      {to_down(curr), :down, 1}
    ]
  end

  def nexts({curr, dir, 3}) when dir in [:top, :down] do
    [
      {to_right(curr), :right, 1},
      {to_left(curr), :left, 1}
    ]
  end

  def nexts({curr, :right, count}),
    do: [
      {to_right(curr), :right, count + 1},
      {to_top(curr), :top, 1},
      {to_down(curr), :down, 1}
    ]

  def nexts({curr, :left, count}),
    do: [
      {to_left(curr), :left, count + 1},
      {to_top(curr), :top, 1},
      {to_down(curr), :down, 1}
    ]

  def nexts({curr, :top, count}),
    do: [
      {to_top(curr), :top, count + 1},
      {to_left(curr), :left, 1},
      {to_right(curr), :right, 1}
    ]

  def nexts({curr, :down, count}),
    do: [
      {to_down(curr), :down, count + 1},
      {to_left(curr), :left, 1},
      {to_right(curr), :right, 1}
    ]

  def to_right({col_idx, row_idx}), do: {col_idx, row_idx + 1}
  def to_left({col_idx, row_idx}), do: {col_idx, row_idx - 1}
  def to_top({col_idx, row_idx}), do: {col_idx - 1, row_idx}
  def to_down({col_idx, row_idx}), do: {col_idx + 1, row_idx}

  def parse() do
    input =
      ~c"input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)

    map =
      input
      |> Enum.with_index()
      |> Enum.flat_map(fn {str, col_idx} ->
        str
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {c, row_idx} -> {{col_idx, row_idx}, String.to_integer(c)} end)
      end)
      |> Map.new()

    {{length(input) - 1, String.length(List.first(input)) - 1}, map}
  end
end

IO.inspect(Day17.part_one())

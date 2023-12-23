defmodule Day23 do
  Mix.install([
    {:libgraph, "~> 0.16.0"}
  ])

  def part_one() do
    {start, target, map} = parse()

    g = build_graph(start, start, target, map, 0, %{}, Graph.new(), &find_nexts_with_slopes/3)

    g
    |> Graph.get_paths(start, target)
    |> Enum.map(&Enum.chunk_every(&1, 2, 1, :discard))
    |> Enum.map(fn path ->
      Enum.reduce(path, 0, fn [a, b], acc ->
        %{weight: weight} = Graph.edge(g, a, b)

        weight + acc
      end)
    end)
    |> Enum.max()
  end

  def build_graph(curr, prev, target, _map, weight, _record, g, _find_nexts)
      when curr == target do
    Graph.add_edges(g, [{prev, curr, weight: weight}])
  end

  def build_graph(curr, prev, target, map, weight, record, g, find_nexts) do
    record = Map.put(record, curr, true)

    find_nexts.(curr, map, record)
    |> case do
      [] ->
        g

      [next] ->
        build_graph(next, prev, target, map, weight + 1, record, g, find_nexts)

      nexts ->
        new_g = Graph.add_edges(g, [{prev, curr, weight: weight}])

        nexts
        |> Enum.reduce(new_g, fn next, acc ->
          build_graph(next, curr, target, map, 1, record, acc, find_nexts)
        end)
    end
  end

  def find_nexts_with_slopes({row_idx, col_idx} = curr, map, record) do
    case map[curr] do
      "#" ->
        []

      ">" ->
        [{row_idx, col_idx + 1}]

      "<" ->
        [{row_idx, col_idx - 1}]

      "v" ->
        [{row_idx + 1, col_idx}]

      _ ->
        [
          {row_idx, col_idx + 1},
          {row_idx, col_idx - 1},
          {row_idx + 1, col_idx},
          {row_idx - 1, col_idx}
        ]
    end
    |> Enum.reject(&is_nil(map[&1]))
    |> Enum.reject(&(map[&1] == "#"))
    |> Enum.reject(&record[&1])
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
# Day23.part_two() |> IO.inspect()

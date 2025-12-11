Mix.install([
  {:libgraph, "~> 0.16.0"}
])

defmodule Solution do
  use Agent

  def start_link(initial_value) do
    Agent.start_link(fn -> initial_value end, name: __MODULE__)
  end

  def get(key) do
    Agent.get(__MODULE__, &Map.get(&1, key))
  end

  def put(key, value) do
    Agent.update(__MODULE__, &Map.put(&1, key, value))
  end

  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ~r/[: ]/, trim: true))
  end

  def part_one(input) do
    g = Graph.new()

    parse(input)
    |> Enum.reduce(g, fn [start | targets], graph ->
      edges =
        for target <- targets do
          {start, target}
        end

      Graph.add_edges(graph, edges)
    end)
    |> Graph.get_paths("you", "out")
    |> length()
    |> IO.inspect(label: "Part One Result")
  end

  # memo = %{{node, seen_fft?, seen_dac?} => count}
  def count_paths(node, seen_fft, seen_dac, map, memo) do
    key = {node, seen_fft, seen_dac}

    case memo do
      %{^key => result} ->
        {result, memo}

      _ ->
        {result, memo} =
          do_count(node, seen_fft, seen_dac, map, memo)

        {result, Map.put(memo, key, result)}
    end
  end

  defp do_count("out", seen_fft, seen_dac, _map, memo) do
    # must contain both
    if seen_fft and seen_dac, do: {1, memo}, else: {0, memo}
  end

  defp do_count(node, seen_fft, seen_dac, map, memo) do
    seen_fft = seen_fft or node == "fft"
    seen_dac = seen_dac or node == "dac"

    Enum.reduce(map[node], {0, memo}, fn next, {acc, memo} ->
      {v, memo} = count_paths(next, seen_fft, seen_dac, map, memo)
      {acc + v, memo}
    end)
  end

  def part_two(input) do
    map =
      parse(input)
      |> Enum.map(fn [start | targets] ->
        {start, targets}
      end)
      |> Map.new()

    {result, _} = count_paths("svr", false, false, map, %{})
    IO.inspect(result, label: "Part Two Result")
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
# Solution.part_one(file_path)
Solution.part_two(file_path)

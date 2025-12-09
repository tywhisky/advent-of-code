Mix.install([
  {:libgraph, "~> 0.16.0"}
])

defmodule Solution do
  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, ",", trim: true))
    |> Enum.map(&Enum.map(&1, fn str -> String.to_integer(str) end))
    |> Enum.map(&List.to_tuple/1)
  end

  def part_one(input) do
    g = Graph.new()

    parse(input)
    |> recursive([], 0, 2, g)
    |> Graph.components()
    |> IO.inspect(label: "Part One Result")
    |> Enum.map(&length/1)
    |> Enum.product()
  end

  def recursive(_, _, count, target, graph) when count >= target do
    graph
  end

  def recursive([], _, _count, _tatget, graph), do: graph

  def recursive([head | tail], rest, count, target, graph) do
    {_, shortest} =
      for i <- Enum.reverse(rest) ++ tail do
        {distance(head, i), i}
      end
      |> Enum.min_by(&elem(&1, 0))

    if Graph.edges(graph, head, shortest) != [] do
      recursive(tail, [head | rest], count, target, graph)
    else
      graph = Graph.add_edge(graph, head, shortest)

      recursive(tail, [head | rest], count + 1, target, graph)
    end
  end

  defp distance({x1, y1, z1}, {x2, y2, z2}) do
    :math.sqrt(
      :math.pow(x2 - x1, 2) +
        :math.pow(y2 - y1, 2) +
        :math.pow(z2 - z1, 2)
    )
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
Solution.part_one(file_path)

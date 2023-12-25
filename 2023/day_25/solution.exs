defmodule Day25 do
  Mix.install([
    {:libgraph, "~> 0.16.0"}
  ])

  def part_one() do
    parse()
    |> build_graph(Graph.new())

  def build_graph([], g), do: g

  def build_graph([[from, to] | tail], g) do
    new_g =
      Enum.reduce(to, g, fn t, acc ->
        Graph.add_edge(acc, from, t)
      end)

    build_graph(tail, new_g)
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn str ->
      [from, to] = String.split(str, ": ", trim: true)
      [from, String.split(to, " ", trim: true)]
    end)
  end
end

Day25.part_one() |> dbg()

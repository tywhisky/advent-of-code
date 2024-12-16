Mix.install([
  {:libgraph, "~> 0.16.0"}
])

defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
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
  end

  def part_one(path) do
    list = parse(path)
    map = Map.new(list)
    {sp, "S"} = Enum.find(list, fn {_, s} -> s == "S" end)
    {ep, "E"} = Enum.find(list, fn {_, e} -> e == "E" end)

    g =
      list
      |> Enum.reduce(Graph.new(), fn
        {{x, y}, a}, g when a in [".", "S", "E"] ->
          edges =
            [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
            |> Enum.filter(&(map[&1] == "." or map[&1] == "S" or map[&1] == "E"))
            |> Enum.map(fn to -> {{x, y}, to} end)

          Graph.add_edges(g, edges)

        _, g ->
          g
      end)

    paths = Graph.get_paths(g, sp, ep)
    vertices = Graph.vertices(g)

    Enum.min_by(paths, fn path ->
      vertices = find_vertices(path)

      "#{vertices}0#{length(path) - 1}"
      |> String.to_integer()
    end)
  end

  defp find_vertices([first | tail], [], result) do
    find_vertices(tail, [{first, :right}], result)
  end
  
  defp find_vertices([curr | tail], [{{rx, ry, direction}} | _] = rest, result) do
    new_direction =
      cond do
        direction == :right and 
      end
  end
end

Solution.part_one("test.txt") |> IO.inspect()

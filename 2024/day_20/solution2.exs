Mix.install([
  {:libgraph, "~> 0.16.0"}
])

defmodule Solution do
  def parse(path) do
    list =
      path
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {row, col_idx} ->
        row
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {c, row_idx} -> {{col_idx, row_idx}, c} end)
      end)

    {sp, _} = Enum.find(list, &(elem(&1, 1) == "S"))
    {ep, _} = Enum.find(list, &(elem(&1, 1) == "E"))
    {sp, ep, list}
  end

  def part_two(path) do
    {sp, ep, list} = parse(path)
    map = Map.new(list)
    normal_seconds = find_normal_path(list, map, sp, ep)

    find_cheat_paths(sp, map, 0, %{}, nil, nil, 0, [], %{})
    |> elem(0)
    |> Enum.map(&(normal_seconds - &1))
    # for test.txt
    |> Enum.filter(&(&1 >= 50))

    #    |> Enum.filter(&(&1 >= 100)) # for input.txt
    |> length()
  end

  def find_cheat_paths(
        {x, y} = curr,
        map,
        path,
        record,
        start_cheat,
        end_cheat,
        cheat_step,
        result,
        cheat_record
      ) do
    new_record = Map.put(record, {x, y}, true)

    [{x + 1, y}, {x - 1, y}, {x, y - 1}, {x, y + 1}]
    |> Enum.reject(&(map[&1] == nil))
    |> Enum.reject(&(record[&1] == true))
    |> Enum.reduce({result, cheat_record}, fn
      next, {r, cr} = acc ->
        case map[next] do
          "#" when start_cheat == nil ->
            find_cheat_paths(
              next,
              map,
              path + 1,
              new_record,
              curr,
              end_cheat,
              cheat_step + 1,
              r,
              cr
            )

          "#" when cheat_step < 20 ->
            find_cheat_paths(
              next,
              map,
              path + 1,
              new_record,
              start_cheat,
              end_cheat,
              cheat_step + 1,
              r,
              cr
            )

          "#" ->
            acc

          "E" ->
            {[path + 1 | r], cr}

          _ when start_cheat != nil ->
            rr = {start_cheat, next}

            if cr[rr] do
              acc
            else
              new_cr = Map.put(cr, rr, true)

              find_cheat_paths(
                next,
                map,
                path + 1,
                new_record,
                start_cheat,
                next,
                cheat_step,
                r,
                new_cr
              )
            end

          _ ->
            find_cheat_paths(
              next,
              map,
              path + 1,
              new_record,
              start_cheat,
              end_cheat,
              cheat_step,
              r,
              cr
            )
        end
    end)
  end

  def find_normal_path(list, map, sp, ep) do
    list
    |> Enum.reduce(Graph.new(), fn {{x, y}, _}, g ->
      edges =
        [{x + 1, y}, {x - 1, y}, {x, y - 1}, {x, y + 1}]
        |> Enum.filter(&(map[&1] in [".", "S", "E"]))
        |> Enum.map(fn to -> {{x, y}, to} end)

      Graph.add_edges(g, edges)
    end)
    |> Graph.dijkstra(sp, ep)
    |> length()
    |> then(fn result -> result - 1 end)
  end
end

Solution.part_two("test.txt") |> IO.inspect()

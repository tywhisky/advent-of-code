defmodule StatefulMap do
  use Agent

  def start_link do
    Agent.start_link(fn -> %{} end, name: __MODULE__)
  end

  def put(key, value) do
    Agent.update(__MODULE__, &Map.put(&1, key, value))
  end

  def get(key) do
    Agent.get(__MODULE__, &Map.get(&1, key))
  end
end

defmodule Solution do
  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, col_id} ->
      row
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, row_id} -> {{col_id, row_id}, c} end)
    end)
  end

  def part_one() do
    map = Map.new(parse())

    parse()
    |> Enum.map(fn {{x, y}, c} ->
      [
        [c, map[{x - 1, y}], map[{x - 2, y}], map[{x - 3, y}]],
        [c, map[{x + 1, y}], map[{x + 2, y}], map[{x + 3, y}]],
        [c, map[{x, y - 1}], map[{x, y - 2}], map[{x, y - 3}]],
        [c, map[{x, y + 1}], map[{x, y + 2}], map[{x, y + 3}]],
        [c, map[{x + 1, y - 1}], map[{x + 2, y - 2}], map[{x + 3, y - 3}]],
        [c, map[{x + 1, y + 1}], map[{x + 2, y + 2}], map[{x + 3, y + 3}]],
        [c, map[{x - 1, y - 1}], map[{x - 2, y - 2}], map[{x - 3, y - 3}]],
        [c, map[{x - 1, y + 1}], map[{x - 2, y + 2}], map[{x - 3, y + 3}]]
      ]
      |> Enum.map(&Enum.join(&1, ""))
      |> Enum.reject(&(&1 != "XMAS" and &1 != "SAMX"))
      |> Enum.count()
    end)
    |> Enum.sum()
    |> div(2)
  end

  def part_two() do
    map = Map.new(parse())

    parse()
    |> Enum.reduce(0, fn
      {{x, y}, "A"}, acc ->
        if check(x, y, map) do
          acc + 1
        else
          acc
        end

      _, acc ->
        acc
    end)
  end

  def check(x, y, map) do
    [map[{x - 1, y - 1}], map[{x - 1, y + 1}], map[{x + 1, y - 1}], map[{x + 1, y + 1}]]
    |> Enum.join("")
    |> case do
      "MSMS" -> true
      "SMSM" -> true
      "SSMM" -> true
      "MMSS" -> true
      _ -> false
    end
  end
end

Solution.part_two() |> dbg(infinity: true)

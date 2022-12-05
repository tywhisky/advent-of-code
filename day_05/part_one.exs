defmodule Day05.PartOne do
  def complete_rows(graph) do
    max_len = Enum.max_by(graph, &length(&1)) |> length()

    Enum.map(graph, fn
      g when length(g) < max_len -> g ++ List.duplicate(" ", max_len - length(g) + 1)
      g -> g
    end)
  end

  def run(crates, [[]]) do
    crates
    |> Enum.to_list()
    |> Enum.sort_by(&elem(&1, 0), :asc)
    |> Enum.map(&elem(&1, 1))
    |> Enum.map(&List.first(&1))
    |> Enum.reject(&is_nil(&1))
    |> Enum.join("")
  end

  def run(crates, [[qty, from, to] | tail_actions]) do
    {take, rest} = Map.get(crates, from) |> Enum.split(qty)

    new_crates =
      crates
      |> Map.put(from, rest)
      |> Map.put(to, Enum.reverse(take) ++ Map.get(crates, to))

    run(new_crates, tail_actions)
  end
end

[crates, actions] = File.read!("./day_05/input.txt") |> String.split("\n\n")

crates =
  crates
  |> String.split("\n")
  |> Enum.reverse()
  |> tl()
  |> Enum.reverse()
  |> Enum.map(&String.replace(&1, "[", " ", global: true))
  |> Enum.map(&String.replace(&1, "]", " ", global: true))
  |> Enum.map(&String.graphemes(&1))
  |> Day05.PartOne.complete_rows()
  |> Enum.zip_with(& &1)
  |> Enum.reject(&Enum.all?(&1, fn x -> x == " " end))
  |> Enum.with_index(1)
  |> Map.new(fn {k, v} ->
    {v, k |> Enum.join("") |> String.trim_leading() |> String.graphemes()}
  end)

actions =
  actions
  |> String.split("\n")
  |> Enum.map(&String.replace(&1, ~r/[a-z]/, ""))
  |> Enum.map(fn num_str ->
    num_str
    |> String.split(" ")
    |> Enum.reject(&(&1 == ""))
    |> Enum.map(&String.to_integer(&1))
  end)

Day05.PartOne.run(crates, actions) |> IO.inspect()

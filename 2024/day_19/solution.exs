defmodule Solution do
  def parse(path) do
    [towels, str] =
      path
      |> File.read!()
      |> String.split("\n\n", trim: true)

    towels =
      towels
      |> String.split(", ", trim: true)
      |> Enum.map(&String.trim/1)
      |> Map.new(&{&1, true})

    list =
      str
      |> String.split("\n", trim: true)
      |> Enum.map(&String.graphemes/1)

    {towels, list}
  end

  def part_one(path) do
    {map, list} = parse(path)

    Enum.map(list, &dfs(&1, map, ""))
    |> Enum.filter(&(&1 == true))
    |> length()
  end

  def dfs([], _map, ""), do: true

  def dfs([], map, path) do
    case map[path] do
      true -> true
      _ -> false
    end
  end

  def dfs([h | tail], map, path) do
    case map[path] do
      true ->
        new_map = Map.delete(map, path)
        dfs(tail, new_map, "") || dfs(tail, map, "#{path}#{h}")

      _ ->
        dfs(tail, map, "#{path}#{h}")
    end
  end
end

Solution.part_one("test.txt") |> IO.inspect()
Solution.part_one("input.txt") |> IO.inspect()

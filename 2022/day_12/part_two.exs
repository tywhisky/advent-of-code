defmodule Day12.PartTwo do
  def run([], _count, _origin, dp, [], _pass) do
    dp
  end

  def run([], count, origin, dp, rest, pass) do
    run(rest, count + 1, origin, dp, [], pass)
  end

  def run([{{x, y}, char} | tail], count, origin, dp, rest, pass) do
    new_pass = Map.put(pass, {x, y}, true)

    nexts =
      [{x + 1, y}, {x - 1, y}, {x, y + 1}, {x, y - 1}]
      |> Enum.reject(&(!origin[&1]))
      |> Enum.reject(&(not is_nil(new_pass[&1])))
      |> Enum.reject(&(origin[&1] - char > 1))
      |> Enum.map(&{&1, origin[&1]})

    new_dp = maintain(nexts, count + 1, dp)

    new_pass =
      Enum.reduce(nexts, new_pass, fn {p, _char}, acc ->
        Map.put(acc, p, true)
      end)

    run(tail, count, origin, new_dp, rest ++ nexts, new_pass)
  end

  def maintain([], _count, dp), do: dp

  def maintain([{p, _char} | tail], count, dp) do
    new_dp = Map.put(dp, p, min(count, dp[p]))
    maintain(tail, count, new_dp)
  end
end

input =
  File.read!("./day_12/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.to_charlist(&1))
  |> Enum.with_index()
  |> Enum.flat_map(fn {list, i} ->
    list
    |> Enum.with_index()
    |> Enum.map(fn {v, j} ->
      {{i, j}, v}
    end)
  end)
  |> Map.new()

{{e_x, e_y}, _} = Enum.find(input, &(elem(&1, 1) == ?E))

input
|> Enum.filter(&(elem(&1, 1) == ?S or elem(&1, 1) == ?a))
|> Enum.flat_map(fn {start_point, _char} ->
  Day12.PartTwo.run([{start_point, ?a}], 0, input, %{}, [], %{start_point => true})
  |> Map.take([{e_x + 1, e_y}, {e_x - 1, e_y}, {e_x, e_y + 1}, {e_x, e_y - 1}])
  |> Enum.map(fn {p, path} ->
    {p, input[p], path}
  end)
  |> Enum.filter(&(elem(&1, 1) in 'yz'))
  |> Enum.map(&(elem(&1, 2) + 1))
end)
|> Enum.min()
|> IO.inspect()

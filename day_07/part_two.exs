defmodule Day07.PartTwo do
  def run([["$", "cd", "/"] | tail], dir_tree, _curr_path) do
    run(tail, dir_tree, [:root])
  end

  def run([["$", "cd", ".."] | tail], dir_tree, curr_path) do
    new_curr_path = Enum.reverse(curr_path) |> tl() |> Enum.reverse()
    run(tail, dir_tree, new_curr_path)
  end

  def run([["$", "cd", dir_name] | tail], dir_tree, curr_path) do
    run(tail, put_in(dir_tree, curr_path ++ [dir_name], %{size: 0}), curr_path ++ [dir_name])
  end

  def run([["$", "ls"] | tail], dir_tree, curr_path) do
    {new_tail, new_dir_tree} = compute(tail, dir_tree, curr_path, 0)
    run(new_tail, new_dir_tree, curr_path)
  end

  def run([], dir_tree, _curr_path) do
    dir_tree
  end

  def in2out(tree, [], _num), do: tree

  def in2out(tree, path, num) do
    parent_path = path |> Enum.reverse() |> tl() |> Enum.reverse()
    origin_size = get_in(tree, parent_path ++ [:size])
    origin_size = origin_size && origin_size || 0
    new_tree = put_in(tree, parent_path ++ [:size], origin_size + num)

    in2out(new_tree, parent_path, num)
  end

  def compute([[""]], dir_tree, curr_path, sum) do
    new_dir_tree = put_in(dir_tree, curr_path ++ [:size], sum)
    {[], in2out(new_dir_tree, curr_path, sum)}
  end

  def compute([["$" | _] | _] = tail, dir_tree, curr_path, sum) do
    new_dir_tree = put_in(dir_tree, curr_path ++ [:size], sum)
    {tail, in2out(new_dir_tree, curr_path, sum)}
  end

  def compute([["dir", dir_name] | tail], dir_tree, curr_path, sum) do
    compute(tail, put_in(dir_tree, curr_path ++ [dir_name], %{size: 0}), curr_path, sum)
  end

  def compute([[size, _] | tail], dir_tree, curr_path, sum) do
    compute(tail, dir_tree, curr_path, sum + String.to_integer(size))
  end

  def dfs(map), do: dfs(Map.to_list(map), [])

  def dfs([], result), do: result

  def dfs([{_k, v} | tail], result) when is_map(v) do
    case Enum.filter(v, &is_map(elem(&1, 1))) do
      [] -> dfs(tail, [v.size | result])
      children -> dfs(children ++ tail, [v.size | result])
    end
  end

  def dfs([{_, v} | tail], result) do
    dfs(tail, [v | result])
  end
end

sizes =
File.read!("./day_07/input.txt")
|> String.split("\n")
|> Enum.map(&String.split(&1, " "))
|> Day07.PartTwo.run(%{root: %{size: 0}}, [:root])
|> Day07.PartTwo.dfs()


max = Enum.max(sizes)

sizes
|> Enum.filter(& max - &1 < 40000000)
|> Enum.min()
|> IO.inspect()

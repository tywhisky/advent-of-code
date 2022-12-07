defmodule Day07.PartOne do
  def run([["$", "cd", "/"] | tail], dir_tree, curr_path) do
    run(tail, dir_tree, [:root])
  end

  def run([["$", "cd", ".."] | tail], dir_tree, [:root]), do: run(tail, dir_tree, [:root])

  def run([["$", "cd", ".."] | tail], dir_tree, curr_path) do
    new_curr_path = Enum.reverse(curr_path) |> tl() |> Enum.reverse()
    run(tail, dir_tree, new_curr_path)
  end

  def run([["$", "cd", dir_name] | tail], dir_tree, curr_path) do
    run(tail, dir_tree, curr_path ++ [dir_name])
  end

  def run([["$", "ls"] | tail], dir_tree, curr_path) do
    curr_dir = get_in(dir_tree, curr_path)
    {new_tail, new_curr_dir} = compute(tail, curr_dir, 0)
    run(new_tail, put_in(dir_tree, curr_path, new_curr_dir), curr_path)
  end

  def run([], dir_tree, curr_path) do
    IO.inspect(dir_tree)
    dfs(dir_tree.root)
  end

  def compute([["$" | _] | _] = tail, curr_dir, sum) do
    {tail, Map.put(curr_dir, :size, sum)}
  end

  def compute([["dir", dir_name] | tail], curr_dir, sum) do
    compute(tail, Map.put(curr_dir, dir_name, %{}), sum)
  end

  def compute([[size, _] | tail], curr_dir, sum) do
    compute(tail, curr_dir, sum + String.to_integer(size))
  end

  def compute([], curr_dir, sum), do: {[], Map.put(curr_dir, :size, sum)}

  def compute([h | tail], curr_dir, sum), do: compute(tail, curr_dir, sum)

  def dfs(tree) do
    maps = Enum.filter(Map.to_list(tree), & is_map(elem(&1, 1))) |> Enum.map(&elem(&1, 1))
    value = Enum.reduce(maps, 0, & (&1.size + &2)) + tree.size
    dfs(maps, [value])
  end

  def dfs([], result), do: result

  def dfs([h | tail], result) do
    maps = Enum.filter(h, & is_map(elem(&1, 1))) |> Enum.map(&elem(&1, 1))
    value = Enum.reduce(maps, 0, & (&1.size + &2)) + h.size
    dfs(maps ++ tail, [value | result])
  end
end

File.read!("./day_07/input.txt")
|> String.split("\n")
|> Enum.take(100)
|> Enum.map(&String.split(&1, " "))
|> Day07.PartOne.run(%{root: %{}}, [:root])
|> Enum.filter(& &1 <= 10_0000)
|> Enum.sum()
|> IO.inspect()

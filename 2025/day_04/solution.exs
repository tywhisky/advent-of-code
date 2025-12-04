defmodule Solution do
  @dirs [
    {-1, -1},
    {-1, 0},
    {-1, 1},
    {0, -1},
    {0, 1},
    {1, -1},
    {1, 0},
    {1, 1}
  ]

  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {line, r} ->
      line
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, c_idx} -> {{r, c_idx}, c} end)
    end)
    |> Map.new()
  end

  def accessible?(map, {r, c}) do
    adj_count = Enum.count(@dirs, fn {dr, dc} -> map[{r + dr, c + dc}] == "@" end)
    adj_count < 4
  end

  def part_one(file) do
    map = parse(file)

    map
    |> Enum.filter(fn {_pos, val} -> val == "@" end)
    |> Enum.count(fn {pos, _} -> accessible?(map, pos) end)
    |> IO.inspect(label: "Part One Result")
  end
  
  def part_two(file) do
    original = parse(file)
    new = recursive(original) |> dbg()

    (original |> Map.to_list() |> Enum.filter(fn {_pos, val} -> val == "@" end) |> length())-
      (new |> Map.to_list() |> length())
    |> dbg()
  end

  def recursive(map) do
    new = 
    map
    |> Enum.filter(fn {_pos, val} -> val == "@" end)
    |> Enum.reject(fn {pos, _} -> accessible?(map, pos) end)
    |> Map.new()

    if new == map, do: new, else: recursive(new)
  end
end

file_path =
  case System.argv() do
    [] -> "test.txt"
    _ -> "input.txt"
  end

# Solution.part_one(file_path)
Solution.part_two(file_path)

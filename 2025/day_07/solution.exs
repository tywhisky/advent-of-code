defmodule Solution do
  use Agent

  def start_link(initial_value) do
    Agent.start_link(fn -> initial_value end, name: __MODULE__)
  end

  def get(key, default_key \\ nil) do
    Agent.get(__MODULE__, &Map.get(&1, key, default_key))
  end

  def put(key, value) do
    Agent.update(__MODULE__, &Map.put(&1, key, value))
  end

  def get_all() do
    Agent.get(__MODULE__, & &1)
  end

  def parse(input) do
    list =
      input
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.with_index()
      |> Enum.flat_map(fn {line, idx} ->
        line
        |> String.graphemes()
        |> Enum.with_index()
        |> Enum.map(fn {char, cidx} -> {{idx, cidx}, char} end)
      end)

    start = Enum.find(list, fn {_pos, char} -> char == "S" end) |> elem(0)
    {start, Map.new(list)}
  end

  def part_one(input) do
    {{x, y}, map} =
      parse(input)

    {:ok, pid} = Solution.start_link(map)

    dfs({x + 1, y}, 0)
    |> IO.inspect(label: "Part One Result")

    Agent.stop(pid)
  end

  def dfs({x, y}, result) do
    case Solution.get({x, y}) do
      nil ->
        result

      "|" ->
        result

      "." ->
        Solution.put({x, y}, "|")
        dfs({x + 1, y}, result)

      "^" ->
        result =
          if Solution.get({x + 1, y - 1}) == "." or Solution.get({x + 1, y + 1}) == ".",
            do: result + 1,
            else: result

        result = dfs({x + 1, y - 1}, result)
        dfs({x + 1, y + 1}, result)
    end
  end

  def part_two(input) do
    {{x, y}, map} =
      parse(input)

    {:ok, pid} = Solution.start_link(%{})

    max_row =
      map
      |> Enum.map(&(elem(&1, 0) |> elem(0)))
      |> Enum.max()

    max_col =
      map
      |> Enum.map(&(elem(&1, 0) |> elem(1)))
      |> Enum.max()

    IO.inspect({max_row, max_col}, label: "Max Rows and Cols")

    for j <- 0..max_col do
      Solution.put({max_row, j}, 1)
    end

    Solution.get_all() |> dbg()

    for i <- (max_row - 1)..0//-1 do
      for j <- max_col..0//-1 do
        case map[{i, j}] do
          "^" ->
            a = Solution.get({i + 1, j - 1}, 0)
            b = Solution.get({i + 1, j + 1}, 0)

            Solution.put({i, j}, a + b)

          str when str in [".", "S"] ->
            Solution.put({i, j}, Solution.get({i + 1, j}, 0))
        end
      end
    end

    Solution.get({x, y}) |> IO.inspect(label: "Part Two Result")
    Agent.stop(pid)
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
Solution.part_one(file_path)
Solution.part_two(file_path)

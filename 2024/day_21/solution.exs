defmodule Solution do
  @numeric_map %{
    "A" => {0, 0},
    "0" => {0, -1},
    "3" => {-1, 0},
    "2" => {-1, -1},
    "1" => {-1, -2},
    "6" => {-2, 0},
    "5" => {-2, -1},
    "4" => {-2, -2},
    "9" => {-3, 0},
    "8" => {-3, -1},
    "7" => {-3, -2}
  }

  @directional_map %{
    "A" => {0, 0},
    ">" => {1, 0},
    "^" => {0, -1},
    "v" => {1, -1},
    "<" => {1, -2}
  }

  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def part_one(path) do
    list = parse(path)

    nums =
      list
      |> Enum.map(&Enum.join(&1, ""))
      |> Enum.map(&extract_number/1)

    lens =
      list
      |> Enum.map(&build_actions(&1, {0, 0}, @numeric_map, []))
      |> Enum.map(&build_actions(&1, {0, 0}, @directional_map, []))
      |> Enum.map(&build_actions(&1, {0, 0}, @directional_map, []))
      |> Enum.map(&Enum.join(&1, ""))

    #   |> Enum.map(&length/1)

    # [nums, lens]
    # |> Enum.zip()
    # |> Enum.map(fn {a, b} -> a * b end)
    # |> Enum.sum()
  end

  def extract_number(str) do
    case Regex.run(~r/\d+/, str) do
      [num] -> String.to_integer(num)
      _ -> nil
    end
  end

  def build_actions([], curr, map, result), do: result

  def build_actions([to | tail], curr, map, result) do
    actions = do_actions(curr, map[to])

    new_result = result ++ actions
    build_actions(tail, map[to], map, new_result)
  end

  def do_actions({x1, y1}, {x2, y2}) do
    up_or_down = x2 - x1
    left_or_right = y2 - y1

    action_x =
      case up_or_down do
        n when n > 0 ->
          List.duplicate("v", up_or_down)

        n when n < 0 ->
          List.duplicate("^", abs(up_or_down))

        0 ->
          []
      end

    action_y =
      case left_or_right do
        n when n > 0 ->
          List.duplicate(">", left_or_right)

        n when n < 0 ->
          List.duplicate("<", abs(left_or_right))

        0 ->
          []
      end

    action_x ++ action_y ++ ["A"]
  end

  def up_or_down(n) do
    List.duplicate("^", n) |> Enum.join("")
  end
end

Solution.part_one("test.txt") |> IO.inspect()

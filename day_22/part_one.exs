defmodule Day22.PartOne do
  def run([], {row, col}, direction, _map) do
    direction_value =
      case direction do
        "R" -> 0
        "D" -> 1
        "L" -> 2
        "U" -> 3
      end

    row * 1000 + 4 * col + direction_value
  end

  def run([h | tail], curr, direction, map) when h in ["L", "R"] do
    new_direction = change_direction(direction, h)
    run(tail, curr, new_direction, map)
  end

  def run([h | tail], curr, direction, map) do
    step = String.to_integer(h)

    new_curr =
      1..step
      |> Enum.reduce(curr, fn _, acc ->
        forward_one_by_one(acc, direction, map)
      end)

    run(tail, new_curr, direction, map)
  end

  defp forward_one_by_one({row, col} = curr, "R", map) do
    case map[{row, col + 1}] do
      nil ->
        {next, _} =
          map |> Enum.filter(&(elem(elem(&1, 0), 0) == row)) |> Enum.min_by(&elem(elem(&1, 0), 1))

        if map[next] == :road do
          next
        else
          curr
        end

      :stone ->
        curr

      :road ->
        {row, col + 1}
    end
  end

  defp forward_one_by_one({row, col} = curr, "U", map) do
    case map[{row - 1, col}] do
      nil ->
        {next, _} =
          map |> Enum.filter(&(elem(elem(&1, 0), 1) == col)) |> Enum.max_by(&elem(elem(&1, 0), 0))

        if map[next] == :road do
          next
        else
          curr
        end

      :stone ->
        curr

      :road ->
        {row - 1, col}
    end
  end

  defp forward_one_by_one({row, col} = curr, "L", map) do
    case map[{row, col - 1}] do
      nil ->
        {next, _} =
          map |> Enum.filter(&(elem(elem(&1, 0), 0) == row)) |> Enum.max_by(&elem(elem(&1, 0), 1))

        if map[next] == :road do
          next
        else
          curr
        end

      :stone ->
        curr

      :road ->
        {row, col - 1}
    end
  end

  defp forward_one_by_one({row, col} = curr, "D", map) do
    case map[{row + 1, col}] do
      nil ->
        {next, _} =
          map |> Enum.filter(&(elem(elem(&1, 0), 1) == col)) |> Enum.min_by(&elem(elem(&1, 0), 0))

        if map[next] == :road do
          next
        else
          curr
        end

      :stone ->
        curr

      :road ->
        {row + 1, col}
    end
  end

  defp change_direction("L", "R"), do: "U"
  defp change_direction("R", "R"), do: "D"
  defp change_direction("U", "R"), do: "R"
  defp change_direction("D", "R"), do: "L"
  defp change_direction("L", "L"), do: "D"
  defp change_direction("R", "L"), do: "U"
  defp change_direction("U", "L"), do: "L"
  defp change_direction("D", "L"), do: "R"
end

[raw_map, raw_path] =
  "#{__DIR__}/example.txt"
  |> File.read!()
  |> String.split("\n\n")

map =
  raw_map
  |> String.split("\n")
  |> Enum.with_index(1)
  |> Enum.reduce(%{}, fn {row, row_index}, acc ->
    row
    |> String.graphemes()
    |> Enum.with_index(1)
    |> Enum.reduce(acc, fn
      {" ", _index}, inner_acc -> inner_acc
      {".", index}, inner_acc -> Map.put(inner_acc, {row_index, index}, :road)
      {"#", index}, inner_acc -> Map.put(inner_acc, {row_index, index}, :stone)
    end)
  end)

{start, _} = Enum.min_by(map, &elem(&1, 0))

path = String.split(raw_path, ~r/[RL]/, include_captures: true)

Day22.PartOne.run(path, start, "R", map)
|> IO.inspect()

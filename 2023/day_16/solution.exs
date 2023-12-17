defmodule Day16 do
  def part_one() do
    map = parse()

    list = move({0, 0}, map[{0, 0}], map, :right, %{})

    0..9
    |> Enum.flat_map(fn c ->
      0..9
      |> Enum.map(fn r -> {{c, r}, 0} end)
    end)
    |> dbg()
  end

  def move(_, nil, _, _, record), do: record

  def move({col_idx, row_idx} = curr, "-", map, direction, record)
      when direction in [:top, :down] do
    IO.inspect(curr, label: "curr /")
    IO.inspect(direction, label: "direction")

    (record[curr] && map[curr] == "." && record) ||
      (
        left = {col_idx, row_idx - 1}
        right = {col_idx, row_idx + 1}
        new_record = Map.put(record, curr, 1)

        new_record =
          move(left, map[left], map, :left, new_record)

        move(right, map[right], map, :right, new_record)
      )
  end

  def move({col_idx, row_idx} = curr, "|", map, direction, record)
      when direction in [:left, :right] do
    IO.inspect(curr, label: "curr /")
    IO.inspect(direction, label: "direction")

    (record[curr] && map[curr] == "." && record) ||
      (
        top = {col_idx - 1, row_idx}
        down = {col_idx + 1, row_idx}
        new_record = Map.put(record, curr, 1)

        new_record =
          move(top, map[top], map, :top, new_record)

        move(down, map[down], map, :down, new_record)
      )
  end

  def move({col_idx, row_idx} = curr, "/", map, direction, record) do
    IO.inspect(curr, label: "curr /")
    IO.inspect(direction, label: "direction")

    (record[curr] && map[curr] == "." && record) ||
      (
        {new_direction, next} =
          case direction do
            :right -> {:top, {col_idx - 1, row_idx}}
            :left -> {:down, {col_idx + 1, row_idx}}
            :top -> {:right, {col_idx, row_idx + 1}}
            :down -> {:left, {col_idx, row_idx - 1}}
          end

        new_record = Map.put(record, curr, 1)
        move(next, map[next], map, new_direction, new_record)
      )
  end

  def move({col_idx, row_idx} = curr, "\\", map, direction, record) do
    IO.inspect(curr, label: "curr \\")
    IO.inspect(direction, label: "direction")

    (record[curr] && map[curr] == "." && record) ||
      (
        {new_direction, next} =
          case direction do
            :right -> {:down, {col_idx + 1, row_idx}}
            :left -> {:top, {col_idx - 1, row_idx}}
            :top -> {:left, {col_idx, row_idx - 1}}
            :down -> {:right, {col_idx, row_idx + 1}}
          end

        # |> dbg()

        new_record = Map.put(record, curr, 1)
        move(next, map[next], map, new_direction, new_record)
      )
  end

  def move({col_idx, row_idx} = curr, _, map, direction, record) do
    IO.inspect(curr, label: "curr /")
    IO.inspect(direction, label: "direction")

    (record[curr] && map[curr] == "." && record) ||
      (
        next =
          case direction do
            :right -> {col_idx, row_idx + 1}
            :left -> {col_idx, row_idx - 1}
            :top -> {col_idx - 1, row_idx}
            :down -> {col_idx + 1, row_idx}
          end

        new_record = Map.put(record, curr, 1)
        move(next, map[next], map, direction, new_record)
      )
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.with_index()
    |> Enum.flat_map(fn {row, col_idx} ->
      row
      |> String.graphemes()
      |> Enum.with_index()
      |> Enum.map(fn {c, row_idx} -> {{col_idx, row_idx}, c} end)
    end)
    |> Map.new()
  end
end

IO.puts("The Result of Day16 Part One is: #{Day16.part_one()}")
# IO.puts("The Result of Day16 Part Two is: #{Day16.part_two()}")

defmodule Day18 do
  def part_one() do
    common(parse())
  end

  def part_two() do
    parse()
    |> Enum.map(fn {_dir, _len, color} ->
      hex = String.slice(color, 1, String.length(color) - 2)

      dir =
        case String.last(color) do
          "0" -> "R"
          "1" -> "D"
          "2" -> "L"
          "3" -> "U"
        end

      {dir, String.to_integer(hex, 16), nil}
    end)
    |> common()
  end

  def common(input) do
    area =
      input
      |> draw({0, 0}, [{0, 0}])
      |> Stream.chunk_every(2, 1, :discard)
      |> Stream.map(fn [{i1, j1}, {i2, j2}] ->
        (i1 - i2) * (j1 + j2)
      end)
      |> Enum.sum()
      |> div(2)
      |> abs()

    perimeter = input |> Enum.map(&elem(&1, 1)) |> Enum.sum()

    area + div(perimeter, 2) + 1
  end

  def draw([], _, result), do: result

  def draw([{dir, len, _color} | tail], curr, result) do
    next = go(curr, dir, len)
    draw(tail, next, [next | result])
  end

  def go({row_idx, col_idx}, "R", len), do: {row_idx, col_idx + len}
  def go({row_idx, col_idx}, "L", len), do: {row_idx, col_idx - len}
  def go({row_idx, col_idx}, "U", len), do: {row_idx - len, col_idx}
  def go({row_idx, col_idx}, "D", len), do: {row_idx + len, col_idx}

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(fn [direction, num_str, color] ->
      {direction, String.to_integer(num_str), String.replace(color, ~r/[()]/, "")}
    end)
  end
end

IO.inspect(Day18.part_one())
IO.inspect(Day18.part_two())

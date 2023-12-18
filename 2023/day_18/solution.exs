defmodule Day18 do
  def part_one() do
    input = parse()

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

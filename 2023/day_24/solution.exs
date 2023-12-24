defmodule Day24 do
  # @min 200_000_000_000_000
  # @max 400_000_000_000_000
  @min 7
  @max 27
  def part_one() do
    list = parse()

    compute(list, [])
    |> dbg()
    |> Enum.filter(fn {x, y} ->
      x >= @min and x <= @max and y >= @min and y <= @max
    end)
    |> length()
  end

  def compute([], result), do: result

  def compute([{m1, b1} | tail], result) do
    new =
      for {m2, b2} <- tail do
        if m1 - m2 != 0 do
          solution_x = (b2 - b1) / (m1 - m2)
          solution_y = m1 * solution_x + b1
          {Float.round(solution_x, 3), Float.round(solution_y, 3)}
        else
          {0, 0}
        end
      end

    compute(tail, new ++ result)
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn str ->
      [points, pos] = String.split(str, " @ ", trim: true)
      [x1, y1, z1] = String.split(points, ", ") |> Enum.map(&String.to_integer/1)

      [px, py, pz] =
        String.replace(pos, " ", "", global: true)
        |> String.split(",")
        |> Enum.map(&String.to_integer/1)

      [x2, y2, z2] = [x1 + px, y1 + py, z1 + pz]

      m = (y2 - y1) / (x2 - x1)
      b = y1 - m * x1
      {m, b}
    end)
  end
end

Day24.part_one() |> IO.inspect()

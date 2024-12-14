Mix.install([
  {:egd, github: "erlang/egd"}
])

defmodule DotMatrixImage do
  def generate_image(matrix, output_path \\ "output.png") do
    cell_size = 10
    width = String.length(Enum.at(matrix, 0)) * cell_size
    height = length(matrix) * cell_size

    image = :egd.create(width, height)
    fill_color = :egd.color(:green)
    background_color = :egd.color(:black)

    :egd.filledRectangle(image, {0, 0}, {width - 1, height - 1}, background_color)

    Enum.with_index(matrix)
    |> Enum.each(fn {row, row_idx} ->
      String.graphemes(row)
      |> Enum.with_index()
      |> Enum.each(fn
        {"#", col_idx} ->
          x1 = col_idx * cell_size
          y1 = row_idx * cell_size
          x2 = x1 + cell_size - 1
          y2 = y1 + cell_size - 1
          :egd.filledRectangle(image, {x1, y1}, {x2, y2}, fill_color)

        _ ->
          :ok
      end)
    end)

    a = :egd.render(image)

    File.write(output_path, a)
  end
end

defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn row ->
      row
      |> String.split(" ", trim: true)
      |> Enum.map(&String.replace(&1, ~r/[p=|v=]/, ""))
      |> Enum.flat_map(fn c ->
        c
        |> String.split(",", trim: true)
        |> Enum.map(&String.to_integer/1)
      end)
    end)
  end

  def part_one(path, wide, tall) do
    path
    |> parse()
    |> Stream.iterate(fn point ->
      one_second(point, wide, tall, [])
    end)
    |> Enum.at(100)
    |> build_quadrant(wide, tall)
    |> Enum.map(&length/1)
    |> Enum.product()
  end

  def part_two(path, wide, tall) do
    list_v = 33..10000 |> Enum.take_every(wide)
    list_h = 87..10000 |> Enum.take_every(tall)

    stream =
      path
      |> parse()
      |> Stream.iterate(fn point ->
        one_second(point, wide, tall, [])
      end)

    (list_v ++ list_h)
    |> Enum.each(fn idx ->
      matrix =
        stream
        |> Enum.at(idx)
        |> display(wide, tall)

      file_name = "#{idx}.png"
      DotMatrixImage.generate_image(matrix, file_name)
    end)

    :ok
  end

  def display(points, wide, tall) do
    map =
      for i <- 0..(wide - 1), j <- 0..(tall - 1) do
        {{i, j}, "."}
      end
      |> Map.new()

    p_map = Map.new(points, fn [a, b, _, _] -> {{a, b}, "#"} end)

    map
    |> Map.merge(p_map)
    |> Enum.group_by(fn {{a, _}, _} -> a end)
    |> Map.to_list()
    |> Enum.sort_by(fn {k, _} -> k end)
    |> Enum.map(fn {_, v} ->
      v
      |> Enum.sort_by(fn {{_, k}, _} -> k end)
      |> Enum.map(&elem(&1, 1))
      |> Enum.join("")
    end)
  end

  def build_quadrant(points, wide, tall) do
    v_mid = div(wide, 2)
    h_mid = div(tall, 2)

    group_1 = Enum.filter(points, fn [x, y, _, _] -> x < v_mid and y < h_mid end)
    group_2 = Enum.filter(points, fn [x, y, _, _] -> x > v_mid and y < h_mid end)
    group_3 = Enum.filter(points, fn [x, y, _, _] -> x < v_mid and y > h_mid end)
    group_4 = Enum.filter(points, fn [x, y, _, _] -> x > v_mid and y > h_mid end)

    [group_1, group_2, group_3, group_4]
  end

  def one_second([], _wide, _tall, result), do: result

  def one_second([[x, y, right, down] | tail], wide, tall, result) do
    new = [teleport(x, right, wide), teleport(y, down, tall), right, down]
    one_second(tail, wide, tall, [new | result])
  end

  def teleport(a, move, len) do
    cond do
      a + move < 0 -> a + move + len
      a + move >= len -> rem(a + move, len)
      true -> a + move
    end
  end
end

# Solution.part_one("test.txt", 11, 7) |> IO.inspect(label: "Part One with test.txt")
# Solution.part_one("input.txt", 101, 103) |> IO.inspect(label: "Part One with input.txt")
# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
Solution.part_two("input.txt", 101, 103)
|> IO.inspect(label: "Part Two with inpt.txt")

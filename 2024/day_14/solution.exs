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
    path
    |> parse()
    |> Stream.iterate(fn point ->
      one_second(point, wide, tall, [])
    end)
    |> Enum.at(100)
    |> display(wide, tall)
  end

  def display(points, wide, tall) do
    v_mid = div(wide, 2)
    h_mid = div(tall, 2)

    group_1 =
      Enum.filter(points, fn [x, y, _, _] -> x < v_mid and y < h_mid end)
      |> Enum.map(fn [x, y, _, _] -> {{x, y}, "#"} end)
      |> Map.new()

    group_2 =
      Enum.filter(points, fn [x, y, _, _] -> x > v_mid and y < h_mid end)
      |> Enum.map(fn [x, y, _, _] -> {{x, y}, "#"} end)
      |> Map.new()

    group_3 =
      Enum.filter(points, fn [x, y, _, _] -> x < v_mid and y > h_mid end)
      |> Enum.map(fn [x, y, _, _] -> {{x, y}, "#"} end)
      |> Map.new()

    group_4 =
      Enum.filter(points, fn [x, y, _, _] -> x > v_mid and y > h_mid end)
      |> Enum.map(fn [x, y, _, _] -> {{x, y}, "#"} end)
      |> Map.new()

    map_1 =
      for i <- 0..(v_mid - 1), j <- 0..(h_mid - 1) do
        {{i, j}, "."}
      end
      |> Map.new()
      |> Map.merge(group_1)
      |> Enum.group_by(fn {a, _b} -> a end)
      |> Enum.group_by(fn {{a, _}, _} -> a end)
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Enum.map(fn {_, v} ->
        v
        |> Enum.sort_by(fn {{_, k}, _} -> k end)
        |> Enum.map(&elem(&1, 1))
        |> Enum.join("")
      end)
      |> IO.inspect()

    map_2 =
      for i <- (v_mid + 1)..(wide - 1), j <- 0..(h_mid - 1) do
        {{i, j}, "."}
      end
      |> Map.new()
      |> Map.merge(group_2)
      |> Enum.group_by(fn {{a, _}, _} -> a end)
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Enum.map(fn {_, v} ->
        v
        |> Enum.sort_by(fn {{_, k}, _} -> k end)
        |> Enum.map(&elem(&1, 1))
        |> Enum.join("")
      end)
      |> IO.inspect()

    map_3 =
      for i <- 0..(v_mid - 1), j <- (h_mid + 1)..(tall - 1) do
        {{i, j}, "."}
      end
      |> Map.new()
      |> Map.merge(group_3)
      |> Enum.group_by(fn {{a, _}, _} -> a end)
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Enum.map(fn {_, v} ->
        v
        |> Enum.sort_by(fn {{_, k}, _} -> k end)
        |> Enum.map(&elem(&1, 1))
        |> Enum.join("")
      end)
      |> IO.inspect()

    map_4 =
      for i <- (v_mid + 1)..(wide - 1), j <- (h_mid + 1)..(tall - 1) do
        {{i, j}, "."}
      end
      |> Map.new()
      |> Map.merge(group_4)
      |> Enum.group_by(fn {{a, _}, _} -> a end)
      |> Map.to_list()
      |> Enum.sort_by(fn {k, _} -> k end)
      |> Enum.map(fn {_, v} ->
        v
        |> Enum.sort_by(fn {{_, k}, _} -> k end)
        |> Enum.map(&elem(&1, 1))
        |> Enum.join("")
      end)
      |> IO.inspect()
  end

  def do_display(map) do
    map
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

Solution.part_one("test.txt", 11, 7) |> IO.inspect(label: "Part One with test.txt")
Solution.part_one("input.txt", 101, 103) |> IO.inspect(label: "Part One with input.txt")
# Solution.part_two("test.txt") |> IO.inspect(label: "Part Two with test.txt")
Solution.part_two("input.txt", 101, 103) |> IO.inspect(label: "Part Two with inpt.txt")

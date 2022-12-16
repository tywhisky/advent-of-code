defmodule Day15.PartOne do
  def build_map([], map) do
    map
    |> Enum.filter(fn {{_, row}, _from} -> row == 2_000_000 end)
    |> Enum.group_by(&elem(&1, 1))
    |> Enum.map(fn {_, list} ->
      Enum.map(list, fn {{need, _}, _} -> need end)
    end)
    |> Enum.reduce(%{}, fn [a, b], acc ->
      a..b
      |> Enum.to_list()
      |> Enum.map(&{&1, 2_000_000})
      |> Enum.concat(acc)
    end)
    |> Enum.uniq()
  end

  def build_map([[sx, sy, bx, by] | tail], map) do
    man_distance = abs(sx - bx) + abs(sy - by)

    right_top =
      Enum.zip(
        Enum.to_list(sx..(sx + man_distance)),
        Enum.to_list((sy - man_distance)..sy)
      )
      |> Enum.map(&{&1, {sx, sy}})

    left_top =
      Enum.zip(
        Enum.to_list(sx..(sx - man_distance)),
        Enum.to_list((sy - man_distance)..sy)
      )
      |> Enum.map(&{&1, {sx, sy}})

    right_bottom =
      Enum.zip(
        Enum.to_list(sx..(sx + man_distance)),
        Enum.to_list((sy + man_distance)..sy)
      )
      |> Enum.map(&{&1, {sx, sy}})

    left_bottom =
      Enum.zip(
        Enum.to_list(sx..(sx - man_distance)),
        Enum.to_list((sy + man_distance)..sy)
      )
      |> Enum.map(&{&1, {sx, sy}})

    new_map = right_top ++ left_top ++ right_bottom ++ left_bottom ++ map

    build_map(tail, new_map)
  end
end

input = File.read!("#{__DIR__}/input.txt")

input =
  Regex.scan(~r/-?[0-9]+/, input)
  |> Enum.map(fn [str] -> String.to_integer(str) end)
  |> Enum.chunk_every(4)

rows = Day15.PartOne.build_map(input, [])

beacons =
  Enum.map(input, fn [a, b, c, d] -> {c, d} end)
  |> Enum.filter(&(elem(&1, 1) == 2_000_000))
  |> Enum.uniq()

(rows -- beacons)
|> length()
|> IO.inspect()

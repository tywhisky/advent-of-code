defmodule Day15.PartTwo do
  @side 4_000_000

  def build_map([], map) do
    find_distress(Enum.to_list(0..@side), map)
  end

  def build_map([[sx, sy, bx, by] | tail], map) do
    man_distance = abs(sx - bx) + abs(sy - by)

    build_map(tail, [{sx, sy, man_distance} | map])
  end

  def find_distress([index | tail], map) do
    Enum.filter(map, fn {_sx, sy, half} ->
      index in (sy - half)..(sy + half)
    end)
    |> Enum.map(fn {sx, sy, half} ->
      [sx - (half - abs(index - sy)), sx + (half - abs(index - sy))]
    end)
    |> Enum.sort()
    |> then(fn list ->
      case disjoint?(list, nil) do
        :ok ->
          find_distress(tail, map)

        {:error, x} ->
          x * @side + index
      end
    end)
  end

  def disjoint?([], _), do: :ok

  def disjoint?([h | tail], nil) do
    disjoint?(tail, h)
  end

  def disjoint?([[cs, ce] = h | tail], [ps, pe] = prev) do
    if Range.disjoint?(cs..ce, ps..pe) && cs - pe > 1 do
      {:error, cs - 1}
    else
      if ce > pe do
        disjoint?(tail, h)
      else
        disjoint?(tail, prev)
      end
    end
  end
end

input = File.read!("#{__DIR__}/input.txt")

input =
  Regex.scan(~r/-?[0-9]+/, input)
  |> Enum.map(fn [str] -> String.to_integer(str) end)
  |> Enum.chunk_every(4)

Day15.PartTwo.build_map(input, [])
|> IO.inspect()

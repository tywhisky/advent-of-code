defmodule Day18.PartTwo do
  def run([x, y, z], _, map, count)
      when x < 0 or y < 0 or z < 0 or
             x > 20 or y > 20 or z > 20,
      do: {[[x, y, z] | map], count}

  def run([x, y, z] = curr, total, map, count) do
    if curr in total do
      {[curr | map], count + 1}
    else
      if curr in map do
        {[curr | map], count}
      else
        map = [curr | map]

        [[x + 1, y, z], [x - 1, y, z], [x, y + 1, z], [x, y - 1, z], [x, y, z + 1], [x, y, z - 1]]
        |> Enum.reduce({map, count}, fn next, {m, c} ->
          run(next, total, m, c)
        end)
      end
    end
  end
end

input =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.split(&1, ","))
  |> Enum.map(&Enum.map(&1, fn word -> String.to_integer(word) end))

Day18.PartTwo.run([0, 0, 0], input, [], 3)
|> IO.inspect()

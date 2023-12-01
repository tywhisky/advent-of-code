defmodule Day18.PartOne do
  def run([_], count), do: count

  def run([[x, y, z] | tail], count) do
    neighbour_qty =
      ([[x + 1, y, z], [x - 1, y, z], [x, y + 1, z], [x, y - 1, z], [x, y, z + 1], [x, y, z - 1]] --
         tail)
      |> length()
      |> then(fn not_existed_qty ->
        6 - not_existed_qty
      end)

    run(tail, count + neighbour_qty)
  end
end

input =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.split(&1, ","))
  |> Enum.map(&Enum.map(&1, fn word -> String.to_integer(word) end))

neighbour_qty = Day18.PartOne.run(input, 0)

(length(input) * 6 - neighbour_qty * 2)
|> IO.inspect()

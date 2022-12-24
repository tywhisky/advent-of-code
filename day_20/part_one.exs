defmodule Day20.PartOne do
  def run([h | tail], map) do

  end
end

list =
File.read!("#{__DIR__}/input.txt")
|> String.split("\n")
|> Enum.map(&String.to_integer/1)

map =
  list
  |> Enum.with_index()
  |> Map.new()


Day20.PartOne.run(list, map)
|> IO.inspect()

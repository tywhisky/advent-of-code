defmodule Day22 do
  def part_one() do
    parse()
  end

  def parse() do
    "test.txt"
    |> File.read!()
    |> dbg()
  end
end

Day22.part_one() |> IO.inspect()

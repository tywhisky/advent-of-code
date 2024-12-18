defmodule Solution do
  def parse(path) do
    [[_, programs] | registers] =
      path
      |> File.read!()
      |> String.split(["\n", "\n\n"], trim: true)
      |> Enum.map(fn row ->
        row
        |> String.split(":", trim: true)
      end)
      |> Enum.reverse()

    programs =
      String.split(programs, ",", trim: true)
      |> Enum.map(&String.replace(&1, " ", ""))
      |> Enum.map(&String.to_integer/1)
      |> Enum.chunk_every(2)

    registers =
      registers
      |> Enum.map(fn [r, n] ->
        [String.last(r), String.replace(n, " ", "") |> String.to_integer()]
      end)

    {programs, registers}
  end

  def part_one(path) do
    {programs, registers} = parse(path)
  end
end

Solution.part_one("test.txt") |> IO.inspect()

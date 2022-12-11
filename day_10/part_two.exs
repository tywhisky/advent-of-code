defmodule Day10.PartTwo do
  def run([], _, _, record), do: record

  def run([:noop | tail], cycle, value, record) do
    run(tail, cycle + 1, value, Map.put(record, cycle, value))
  end

  def run([{:addx, v} | tail], cycle, value, record) do
    new_record =
      record
      |> Map.put(cycle, value)
      |> Map.put(cycle + 1, v + value)

    run(tail, cycle + 2, value + v, new_record)
  end
end

input =
  File.read!("#{__DIR__}/input.txt")
  |> String.split("\n")
  |> Enum.map(&String.replace(&1, "\r", ""))
  |> Enum.map(fn str ->
    case String.split(str, " ") do
      ["addx", value] -> {:addx, String.to_integer(value)}
      ["noop"] -> :noop
    end
  end)

Day10.PartTwo.run(input, 1, 1, %{})
|> Map.to_list()
|> Enum.sort_by(&elem(&1, 0))
|> IO.inspect(limit: :infinity)
|> Enum.map(fn {cycle, value} ->
  if rem(cycle, 40) in (value - 1)..(value + 1), do: "#", else: "."
end)
|> Enum.chunk_every(40)
|> Enum.map(&Enum.join(&1, ""))
|> IO.inspect()

defmodule Day10.PartOne do
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

record = Day10.PartOne.run(input, 1, 1, %{})

[20, 60, 100, 140, 180, 220]
|> Enum.reduce(0, & &2 + Map.get(record, &1 - 1) * &1)
|> IO.inspect
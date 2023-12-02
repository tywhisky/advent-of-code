defmodule Day02 do
  def part_one() do
    input()
    |> parse()
    |> Enum.reduce(0, fn %{id: id, rounds: rounds}, acc ->
      (Enum.all?(rounds, fn round ->
         Map.get(round, "red", 0) <= 12 and Map.get(round, "green", 0) <= 13 and
           Map.get(round, "blue", 0) <= 14
       end) &&
         acc + id) ||
        acc
    end)
  end

  def part_two() do
    input()
    |> parse()
    |> Enum.reduce(0, fn %{rounds: rounds}, acc ->
      power =
        rounds
        |> Enum.reduce(%{"blue" => 0, "green" => 0, "red" => 0}, fn round, acc ->
          Map.merge(acc, round, fn _key, val1, val2 -> max(val1, val2) end)
        end)
        |> Enum.map(&elem(&1, 1))
        |> Enum.product()

      power + acc
    end)
  end

  def input() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
  end

  def parse(data) do
    data
    |> Enum.map(fn line ->
      [game, tail] = String.split(line, ":")

      id =
        game
        |> String.replace(~r/[^0-9]/, "")
        |> String.to_integer()

      rounds =
        tail
        |> String.slice(1..-1)
        |> String.split(";")
        |> Enum.map(fn list ->
          list
          |> String.split(",")
          |> Enum.map(&String.split(&1, " ", trim: true))
          |> Enum.map(fn [qty, color] -> {color, String.to_integer(qty)} end)
          |> Map.new()
        end)

      %{id: id, rounds: rounds}
    end)
  end
end

IO.puts("The part one's result: #{Day02.part_one()}")
IO.puts("The part two's result: #{Day02.part_two()}")

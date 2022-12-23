defmodule Day19.PartOne do
  def run([index | costs]) do
    simulate(1, costs, [1, 0, 0, 0], [0, 0, 0, 0])
    IO.inspect(Process.get(:max_geodes, 0), label: "index: #{index}")
    index * Process.get(:max_geodes, 0)
  end

  def simulate(25, _, _, [_, _, _, gev]) do
    Process.put(:max_geodes, max(Process.get(:max_geodes, 0), gev))
  end

  def simulate(
        minutes,
        [
          ore_cost,
          clay_cost,
          obsidian_cost_ore,
          obsidian_cost_clay,
          geode_cost_ore,
          geode_cost_obsidian
        ] = cost,
        [ore, clay, obsidian, geode],
        [orv, clv, obv, gev]
      ) do
    remaining = 25 - minutes
    triangular = div(remaining * (remaining + 1), 2)
    max_score = gev + geode * remaining + triangular
    Process.put(:max_geodes, max(Process.get(:max_geodes, 0), gev))

    if max_score < Process.get(:max_geodes, 0) do
      gev
    else
      list = [
        {[ore, clay, obsidian, geode], [orv + ore, clv + clay, obv + obsidian, gev + geode]}
      ]

      list =
        if orv >= geode_cost_ore and obv >= geode_cost_obsidian do
          [
            {[ore, clay, obsidian, geode + 1],
             [
               orv + ore - geode_cost_ore,
               clv + clay,
               obv + obsidian - geode_cost_obsidian,
               gev + geode
             ]}
            | list
          ]
        else
          list
        end

      list =
        if orv >= obsidian_cost_ore and clv >= obsidian_cost_clay and
             obsidian < geode_cost_obsidian do
          [
            {[ore, clay, obsidian + 1, geode],
             [
               orv + ore - obsidian_cost_ore,
               clv + clay - obsidian_cost_clay,
               obv + obsidian,
               gev + geode
             ]}
            | list
          ]
        else
          list
        end

      list =
        if orv >= clay_cost and clay < obsidian_cost_clay do
          [
            {[ore, clay + 1, obsidian, geode],
             [orv + ore - clay_cost, clv + clay, obv + obsidian, gev + geode]}
            | list
          ]
        else
          list
        end

      list =
        if orv >= ore_cost and
             (ore < ore_cost or ore < clay_cost or ore < obsidian_cost_ore or ore < geode_cost_ore) do
          [
            {[ore + 1, clay, obsidian, geode],
             [orv + ore - ore_cost, clv + clay, obv + obsidian, gev + geode]}
            | list
          ]
        else
          list
        end

      list
      |> Enum.reduce(gev, fn {qty, values}, acc ->

        max(acc, simulate(minutes + 1, cost, qty, values))
      end)
    end
  end
end

input = File.read!("#{__DIR__}/input.txt")

Regex.scan(~r/[0-9]+/, input)
|> Enum.map(fn [num] -> String.to_integer(num) end)
|> Enum.chunk_every(7)
|> Task.async_stream(&Day19.PartOne.run(&1), timeout: :infinity)
|> Enum.map(&elem(&1, 1))
|> Enum.sum()
|> IO.inspect()

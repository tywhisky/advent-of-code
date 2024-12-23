defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.to_integer/1)
  end

  def part_one(path) do
    parse(path)
    |> Stream.iterate(fn secrets ->
      Enum.map(secrets, fn secret ->
        do_round(secret)
      end)
    end)
    |> Enum.at(2000)
    |> Enum.sum()
  end

  def do_round(secret) do
    secret =
      (secret * 64)
      |> mix(secret)
      |> prune()

    secret =
      div(secret, 32)
      |> round()
      |> mix(secret)
      |> prune()

    (secret * 2048)
    |> mix(secret)
    |> prune()
  end

  def mix(value, secret) do
    Bitwise.bxor(round(value), round(secret))
  end

  def prune(secret) do
    rem(secret, 16_777_216)
  end
end

Solution.part_one("test.txt") |> IO.inspect()
Solution.part_one("input.txt") |> IO.inspect()

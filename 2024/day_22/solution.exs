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

  def part_two(path) do
    parse(path)
    |> Enum.map(fn x -> [x] end)
    |> Stream.iterate(fn secrets ->
      Enum.map(secrets, fn [h | _] = acc ->
        [do_round(h) | acc]
      end)
    end)
    |> Enum.at(2000 - 1)
    |> Enum.map(&Enum.map(&1, fn x -> rem(x, 10) end))
    |> Enum.flat_map(fn list ->
      diffs =
        list
        |> Enum.chunk_every(2, 1, :discard)
        |> Enum.map(fn [a, b] -> a - b end)
        |> Enum.chunk_every(4, 1, :discard)

      Enum.zip(diffs, list)
      |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
      |> Enum.map(fn {a, b} -> {a, Enum.max(b)} end)
    end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
    |> Enum.max_by(fn {a, b} -> Enum.sum(b) end)
    |> elem(1)
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

# Solution.part_one("test.txt") |> IO.inspect()
# Solution.part_one("input.txt") |> IO.inspect()
Solution.part_two("test.txt") |> IO.inspect()
Solution.part_two("input.txt") |> IO.inspect()

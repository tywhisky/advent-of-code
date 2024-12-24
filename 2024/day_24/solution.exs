defmodule Solution do
  require Bitwise
  alias Cache

  def parse(path) do
    [initial, wires] =
      path
      |> File.read!()
      |> String.split(["\n\n"], trim: true)

    initial =
      initial
      |> String.split(["\n", ": "], trim: true)
      |> Enum.chunk_every(2)
      |> Map.new(fn [a, b] -> {a, String.to_integer(b)} end)

    wires =
      wires
      |> String.split(["\n", " -> "], trim: true)
      |> Enum.chunk_every(2)
      |> Enum.map(fn [a, b] ->
        [x, action, y] = String.split(a, " ", trim: true)

        {b, {build_fun(action), [x, y]}}
      end)

    {initial, wires}
  end

  defp build_fun("AND"), do: fn a, b -> Bitwise.band(a, b) end
  defp build_fun("XOR"), do: fn a, b -> Bitwise.bxor(a, b) end
  defp build_fun("OR"), do: fn a, b -> Bitwise.bor(a, b) end

  def part_one(path) do
    {initial, wires} = parse(path)

    Stream.iterate({wires, initial}, fn {ws, map} ->
      new_map =
        Enum.reduce(ws, map, fn {to, {fun, [x, y]}}, acc ->
          x = acc[x]
          y = acc[y]

          if x != nil and y != nil do
            Map.put(acc, to, apply(fun, [x, y]))
          else
            acc
          end
        end)

      {ws, new_map}
    end)
    |> Enum.at(100)
    |> elem(1)
    |> Map.to_list()
    |> Enum.filter(&String.starts_with?(elem(&1, 0), "z"))
    |> Enum.sort(:desc)
    |> Enum.map(&elem(&1, 1))
    |> Enum.join("")
    |> String.to_integer(2)
  end
end

Solution.part_one("test.txt") |> IO.inspect()
Solution.part_one("input.txt") |> IO.inspect()

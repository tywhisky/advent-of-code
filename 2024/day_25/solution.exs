defmodule Solution do
  def parse(path) do
    path
    |> File.read!()
    |> String.split("\n\n", trim: true)
    |> Enum.map(&String.split(&1, "\n", trim: true))
    |> Enum.map(&Enum.map(&1, fn row -> String.graphemes(row) end))
    |> Enum.map(&Enum.zip/1)
    |> Enum.map(fn key ->
      direction = if Enum.all?(key, &(elem(&1, 0) == "#")), do: :left, else: :right

      key =
        Enum.map(key, fn tuple ->
          Tuple.to_list(tuple)
          |> Enum.count(&(&1 == "#"))
          |> then(fn x -> x - 1 end)
        end)

      {direction, key}
    end)
    |> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
  end

  def part_one(path) do
    %{left: left, right: right} = parse(path)

    for l <- left, r <- right do
      Enum.zip_with([l, r], fn [x, y] -> x + y end)
    end
    |> Enum.count(&Enum.all?(&1, fn x -> x <= 5 end))
  end
end

Solution.part_one("test.txt") |> IO.inspect()
Solution.part_one("input.txt") |> IO.inspect()

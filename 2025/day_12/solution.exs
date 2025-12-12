defmodule Solution do
  def parse(input) do
    [regions | shapes] =
      input
      |> File.read!()
      |> String.split("\n\n", trim: true)
      |> Enum.reverse()

    processed_regions =
      regions
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, ~r/[ :x]/, trim: true))
      |> Enum.map(fn [wide, long | shape_idxs] ->
        {String.to_integer(wide), String.to_integer(long),
         shape_idxs |> Enum.map(&String.to_integer/1) |> Enum.with_index(&{&2, &1})}
      end)

    processed_shapes =
      shapes
      |> Enum.map(&String.split(&1, ~r/[:\n]/, trim: true))
      |> Enum.map(fn [idx_str | shape_lines] ->
        {String.to_integer(idx_str), shape_lines}
      end)
      |> Map.new()

    {processed_regions, processed_shapes}
  end

  def part_one(input) do
    {regions, shapes} =
      parse(input)

    shapes_size_map =
      shapes
      |> Enum.map(fn {idx, shape_lines} ->
        size =
          shape_lines
          |> Enum.map(&String.graphemes/1)
          |> Enum.map(&Enum.count(&1, fn c -> c == "#" end))
          |> Enum.sum()

        {idx, size}
      end)
      |> Map.new()

    regions
    |> dbg()
    |> Enum.count(fn {wide, long, shape_idxs} ->
      area = wide * long
      # |> dbg()

      total_shape_size =
        shape_idxs
        |> Enum.map(fn {shape_idx, repeat} -> Map.get(shapes_size_map, shape_idx) * repeat end)
        |> Enum.sum()

      #        |> dbg()

      area * 0.85 > total_shape_size
    end)
    |> IO.inspect(label: "Part One Result")
  end

  def part_two() do
    IO.inspect("Part Two Result")
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
Solution.part_one(file_path)

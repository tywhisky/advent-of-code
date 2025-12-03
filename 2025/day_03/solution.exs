defmodule Solution do
  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end

  def part_one(input) do
    parse(input)
    |> Enum.map(fn line ->
      [{fst, fst_idx}, {snd, snd_idx} | rest] =
        line
        |> Enum.map(&String.to_integer/1)
        |> Enum.with_index()
        |> Enum.sort_by(&elem(&1, 0), :desc)

      backup = Enum.find(rest, &(elem(&1, 1) > fst_idx))

      cond do
        fst_idx < snd_idx ->
          fst * 10 + snd

        backup != nil ->
          fst * 10 + elem(backup, 0)

        true ->
          snd * 10 + fst
      end
    end)
    |> Enum.sum()
    |> IO.inspect(label: "Part One Result")
  end
end

file_path =
  case System.argv() do
    [] -> "test.txt"
    _ -> "input.txt"
  end

Solution.part_one(file_path)

defmodule Solution do
  @k 12
  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
  end

  def part_one(input) do
    parse(input)
    |> Enum.map(&String.graphemes/1)
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

  def part_two(input) do
    parse(input)
    |> Enum.map(&pick_max_12/1)
    |> Enum.map(&String.to_integer/1)
    |> Enum.sum()
    |> dbg()
  end

  def pick_max_12(digits) do
    digits = String.graphemes(digits)
    n = length(digits)
    cache = :ets.new(:dp_cache, [:set, :public])

    res =
      dp(0, @k, digits, n, cache)

    :ets.delete(cache)
    res
  end

  defp dp(_i, 0, _digits, _n, _cache), do: ""
  defp dp(i, j, _digits, n, _cache) when n - i < j, do: nil

  defp dp(i, j, digits, n, cache) do
    case :ets.lookup(cache, {i, j}) do
      [{_, v}] ->
        v

      [] ->
        max_pos = n - j

        {best_digit, best_index} =
          i..max_pos
          |> Enum.map(&{Enum.at(digits, &1), &1})
          |> Enum.max_by(fn {d, idx} -> {d, -idx} end)

        tail = dp(best_index + 1, j - 1, digits, n, cache)

        res =
          case tail do
            nil -> nil
            _ -> best_digit <> tail
          end

        :ets.insert(cache, {{i, j}, res})
        res
    end
end
end

file_path =
  case System.argv() do
    [] -> "test.txt"
    _ -> "input.txt"
  end

# Solution.part_one(file_path)
Solution.part_two(file_path)

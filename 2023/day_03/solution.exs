defmodule Day03 do
  def part_one() do
    coords =
      input()
      |> Enum.map(&String.graphemes/1)
      |> Enum.map(&Enum.with_index/1)
      |> Enum.with_index()

    nums =
      coords
      |> Enum.reduce([], fn {row, col_idx}, acc ->
        point =
          search_nums(row, {nil, []}, [])
          |> Enum.map(fn {num_str, idxs} ->
            {
              String.to_integer(num_str),
              Enum.map(idxs, &{&1, col_idx})
            }
          end)

        point ++ acc
      end)

    map =
      coords
      |> Enum.flat_map(fn {row, col_idx} ->
        Enum.map(row, fn {str, row_idx} -> {{row_idx, col_idx}, str} end)
      end)
      |> Enum.filter(&String.match?(elem(&1, 1), ~r/[^1-9]/))
      |> Enum.filter(&String.match?(elem(&1, 1), ~r/[^.]/))
      |> Map.new()

    nums
    |> Enum.filter(fn {num, points} ->
      Enum.any?(points, fn {x, y} ->
        Map.get(map, {x + 1, y}) ||
          Map.get(map, {x - 1, y}) ||
          Map.get(map, {x, y + 1}) ||
          Map.get(map, {x, y - 1}) ||
          Map.get(map, {x - 1, y - 1}) ||
          Map.get(map, {x - 1, y + 1}) ||
          Map.get(map, {x + 1, y - 1}) ||
          Map.get(map, {x + 1, y + 1})
      end)
    end)
    |> Enum.map(&elem(&1, 0))
    |> Enum.sum()
    |> dbg()
  end

  defp search_nums([], prev, result), do: [prev | result] |> tl()

  defp search_nums([{char, idx} | tail], {prev_str, prev_idxs}, result) do
    if String.match?(char, ~r/[1-9]/) do
      search_nums(tail, {"#{prev_str}#{char}", [idx | prev_idxs]}, result)
    else
      if not is_nil(prev_str) do
        search_nums(tail, {nil, []}, [{prev_str, Enum.reverse(prev_idxs)} | result])
      else
        search_nums(tail, {nil, []}, result)
      end
    end
  end

  def input() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
  end
end

Day03.part_one()

defmodule Day03 do
  def part_one() do
    coords = make_coords()
    nums = make_nums(coords)

    map =
      coords
      |> Enum.flat_map(fn {row, col_idx} ->
        Enum.map(row, fn {str, row_idx} -> {{row_idx, col_idx}, str} end)
      end)
      |> Enum.filter(&String.match?(elem(&1, 1), ~r/[^0-9]/))
      |> Enum.filter(&String.match?(elem(&1, 1), ~r/[^.]/))
      |> Map.new()

    nums
    |> Enum.filter(fn {_num, points} ->
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
  end

  def part_two() do
    coords = make_coords()

    nums_map =
      coords
      |> make_nums()
      |> Enum.flat_map(fn {num, points} ->
        Enum.map(points, &{{elem(&1, 1), elem(&1, 0)}, num})
      end)
      |> Map.new()

    coords
    |> Enum.flat_map(fn {row, col_idx} ->
      Enum.map(row, fn {str, row_idx} -> {{col_idx, row_idx}, str} end)
    end)
    |> Enum.filter(&String.match?(elem(&1, 1), ~r/[*]/))
    |> Enum.map(fn {{x, y}, "*"} ->
      [
        {x + 1, y},
        {x - 1, y},
        {x, y + 1},
        {x, y - 1},
        {x + 1, y + 1},
        {x + 1, y - 1},
        {x - 1, y + 1},
        {x - 1, y - 1}
      ]
      |> Enum.map(&Map.get(nums_map, &1))
      |> Enum.reject(&is_nil/1)
      |> Enum.uniq()
      |> then(fn list ->
        if length(list) == 2, do: Enum.product(list), else: 0
      end)
    end)
    |> Enum.sum()
  end

  defp make_nums(coords) do
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
  end

  defp make_coords() do
    input()
    |> Enum.map(&String.graphemes/1)
    |> Enum.map(&Enum.with_index/1)
    |> Enum.with_index()
  end

  defp search_nums([], prev, result), do: [prev | result] |> Enum.reject(& elem(&1, 0) == nil)

  defp search_nums([{char, idx} | tail], {prev_str, prev_idxs}, result) do
    if String.match?(char, ~r/[0-9]/) do
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

IO.inspect("The Result of Day03 Part One: #{Day03.part_one()}")
IO.inspect("The Result of Day03 Part Two: #{Day03.part_two()}")

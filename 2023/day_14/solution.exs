defmodule Day14 do
  def part_one() do
    parse()
    |> origin_to_north()
    |> sum()
  end

  def part_two() do
    list = parse()

    {start_cycle_idx, end_cycle_idx} =
      0..500
      |> Range.to_list()
      |> find_cycle(list, %{})

    mod =
      rem(1_000_000_000 - start_cycle_idx + 1, end_cycle_idx - start_cycle_idx + 1)

    1..(end_cycle_idx + mod)
    |> Range.to_list()
    |> Enum.reduce(list, fn _, acc -> do_one_cycle(acc) end)
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> sum()
  end

  def sum(list) do
    list
    |> Enum.map(&Enum.reverse/1)
    |> Enum.map(&Enum.with_index(&1, 1))
    |> Enum.map(
      &(Enum.map(&1, fn
          {"O", idx} -> idx
          _ -> 0
        end)
        |> Enum.sum())
    )
    |> Enum.sum()
  end

  def find_cycle([idx | tail], list, record) do
    new_list = do_one_cycle(list)
    new_record = Map.put(record, list, idx)

    (new_record[new_list] && {new_record[new_list], idx}) ||
      find_cycle(tail, new_list, new_record)
  end

  def do_one_cycle(list) do
    list
    |> origin_to_north()
    |> north_to_west()
    |> west_to_south()
    |> south_to_east()
  end

  def origin_to_north(list) do
    list
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      list =
        tuple
        |> Tuple.to_list()
        |> Enum.with_index()

      rocks_to_left(list, [], list)
    end)
  end

  def north_to_west(list), do: origin_to_north(list)

  def west_to_south(list) do
    list
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      list =
        tuple
        |> Tuple.to_list()
        |> Enum.reverse()
        |> Enum.with_index()

      rocks_to_left(list, [], list) |> Enum.reverse()
    end)
  end

  def south_to_east(list), do: west_to_south(list)

  def rocks_to_left([], _, list), do: Enum.map(list, &elem(&1, 0))

  def rocks_to_left([{"#", _idx} | tail], _queue, list) do
    rocks_to_left(tail, [], list)
  end

  def rocks_to_left([{"O", _idx} | tail], [], list) do
    rocks_to_left(tail, [], list)
  end

  def rocks_to_left([{"O", idx} | tail], [h | tail_queue], list) do
    new_list =
      list
      |> List.replace_at(h, {"O", h})
      |> List.replace_at(idx, {".", idx})

    new_queue = [idx | Enum.reverse(tail_queue)] |> Enum.reverse()

    rocks_to_left(tail, new_queue, new_list)
  end

  def rocks_to_left([{".", idx} | tail], queue, list) do
    new_queue = [idx | Enum.reverse(queue)] |> Enum.reverse()
    rocks_to_left(tail, new_queue, list)
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.graphemes/1)
  end
end

IO.puts("The Result of Day14 Part One is: #{Day14.part_one()}")
IO.puts("The Result of Day14 Part Two is: #{Day14.part_two()}")

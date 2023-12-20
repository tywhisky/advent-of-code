defmodule Day20 do
  def part_one() do
    {map, record} = parse()
    result = %{low: 0, high: 0}

    recursive(1000, map, record, result)
    |> Enum.map(&elem(&1, 1))
    |> Enum.product()
  end

  def recursive(0, map, record, result), do: result

  def recursive(count, map, record, result) do
    # IO.inspect("____________________")
    start = "broadcaster"
    {next_record, next_result} = do_work(start, "button", map[start], :low, map, record, result)
    recursive(count - 1, map, next_record, next_result)
  end

  def do_work(key, _, nil, pulse, map, record, result) do
    # IO.inspect({key, pulse})
    new_result = Map.update!(result, pulse, &(&1 + 1))
    {record, new_result}
  end

  def do_work(key, _from, {:broadcaster, nexts}, pulse, map, record, result) do
    # IO.inspect({key, pulse})

    nexts
    |> Enum.reduce({record, result}, fn next, {acc_record, acc_result} ->
      new_acc_result = Map.update!(acc_result, pulse, &(&1 + 1))

      do_work(next, key, map[next], pulse, map, acc_record, acc_result)
    end)
  end

  def do_work(key, _from, {:flip, nexts}, pulse, map, record, result) do
    # IO.inspect({key, pulse})

    nexts
    |> Enum.reduce({record, result}, fn next, {acc_record, acc_result} ->
      turn? = !!record[key]
      new_acc_result = Map.update!(acc_result, pulse, &(&1 + 1))

      cond do
        pulse == :low && turn? == false ->
          new_acc_record = Map.put(acc_record, key, true)
          new_pulse = :high

          do_work(next, key, map[next], new_pulse, map, new_acc_record, new_acc_result)

        pulse == :low && turn? == true ->
          new_acc_record = Map.put(acc_record, key, false)
          new_pulse = :low

          do_work(next, key, map[next], new_pulse, map, new_acc_record, new_acc_result)

        # pulse == :high && turn? == false ->
        true ->
          {acc_record, new_acc_result}
          # do_work(next, key, map[next], pulse, map, acc_record, new_acc_result)
      end
    end)
  end

  def do_work(key, from, {:con, nexts}, pulse, map, record, result) do
    # IO.inspect({key, pulse})

    nexts
    |> Enum.reduce({record, result}, fn next, {acc_record, acc_result} ->
      new_acc_result = Map.update!(acc_result, pulse, &(&1 + 1))

      new_acc_record =
        Map.update!(acc_record, key, fn map ->
          Map.put(map, from, pulse)
        end)

      new_pulse =
        (Enum.all?(acc_record[key], fn {_, v} -> v == :high end) && :low) ||
          :high

      # IO.inspect("++++++++++++")
      # IO.inspect(key, label: "key")
      # IO.inspect(from, label: "from")
      # IO.inspect(new_acc_record, label: "record")
      # IO.inspect(pulse, label: "pulse")
      # IO.inspect(new_pulse, label: "new_pulse")
      # IO.inspect(next, label: "next")

      do_work(next, key, map[next], new_pulse, map, new_acc_record, new_acc_result)
    end)
  end

  def parse() do
    map =
      "input.txt"
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.map(&String.split(&1, " -> ", trim: true))
      |> Map.new(fn [key, value] ->
        case String.graphemes(key) do
          ["%" | tail] -> {Enum.join(tail), {:flip, String.split(value, ", ", trim: true)}}
          ["&" | tail] -> {Enum.join(tail), {:con, String.split(value, ", ", trim: true)}}
          _ -> {key, {:broadcaster, String.split(value, ", ", trim: true)}}
        end
      end)

    list = Map.to_list(map)

    cons =
      list
      |> Enum.filter(fn {_, {type, _}} -> type == :con end)
      |> Enum.map(&elem(&1, 0))

    cons_record =
      list
      |> Enum.filter(fn {key, {type, nexts}} ->
        t = nexts -- nexts -- cons
        t != []
      end)
      |> Enum.flat_map(fn {key, {_, nexts}} ->
        (nexts -- nexts -- cons)
        |> Enum.reduce([], fn x, acc ->
          [{x, key} | acc]
        end)
      end)
      |> Enum.group_by(&elem(&1, 0), &{elem(&1, 1), :low})
      |> Map.new(fn {k, v} -> {k, Map.new(v)} end)

    {map, cons_record}
  end
end

Day20.part_one() |> IO.inspect()

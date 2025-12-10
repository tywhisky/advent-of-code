defmodule Solution do
  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(fn str ->
      str
      |> String.split(",", trim: true)
      |> then(fn [a, b] -> {String.to_integer(a), String.to_integer(b)} end)
    end)
  end

  def part_one(input) do
    parse(input)
    |> recursive([])
    |> Enum.max()
    |> IO.inspect(label: "Part One Result")
  end

  def recursive([_], result), do: result

  def recursive([{a, b} | tail], result) do
    max =
      for {x, y} <- tail do
        (abs(a - x) + 1) * (abs(b - y) + 1)
      end
      |> Enum.max()

    recursive(tail, [max | result])
  end

  def part_two(input) do
    reds = parse(input)

    map =
      reds
      |> Enum.chunk_every(2, 1, :discard)
      |> Enum.filter(fn
        [{_a, b}, {_c, d}] -> b == d
      end)
      |> Enum.map(fn [{a, b}, {c, _d}] ->
        {b, {a, c}}
      end)
      |> Map.new()

    col_map =
      reds
      |> Enum.group_by(&elem(&1, 0))
      |> Enum.map(fn {k, list} ->
        a =
          list
          |> Enum.map(&elem(&1, 1))
          |> Enum.min_max()

        {k, a}
      end)
      |> Map.new()
      |> IO.inspect(label: "Col Map")

    {min_row, max_row} =
      map
      |> Map.keys()
      |> Enum.min_max()
      |> dbg()

    {min_col, max_col} =
      col_map
      |> Map.keys()
      |> Enum.min_max()
      |> dbg()

    {_, right_to_left} =
      Enum.reduce(max_col..min_col//-1, {nil, %{}}, fn
        col, {nil, acc} ->
          case Map.get(col_map, col) do
            nil ->
              {nil, acc}

            {s, e} ->
              a = {s, e} |> Tuple.to_list() |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, col, a)}
          end

        col, {{xx, yy} = curr, acc} ->
          case Map.get(col_map, col) do
            nil ->
              {curr, Map.put(acc, col, curr)}

            {s, e} ->
              [ss, ee] =
                {s, e}
                |> Tuple.to_list()
                |> Enum.sort()

              a = [min(xx, ss), max(yy, ee)] |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, col, a)}
          end
      end)
      |> IO.inspect(label: "Right to Left")

    {_, left_to_right} =
      Enum.reduce(min_col..max_col, {nil, %{}}, fn
        col, {nil, acc} ->
          case Map.get(col_map, col) do
            nil ->
              {nil, acc}

            {s, e} ->
              a = {s, e} |> Tuple.to_list() |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, col, a)}
          end

        col, {{xx, yy} = curr, acc} ->
          case Map.get(col_map, col) do
            nil ->
              {curr, Map.put(acc, col, curr)}

            {s, e} ->
              [ss, ee] =
                {s, e}
                |> Tuple.to_list()
                |> Enum.sort()

              a = [min(xx, ss), max(yy, ee)] |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, col, a)}
          end
      end)
      |> IO.inspect(label: "Left to Right")

    {_, top_to_bottom} =
      Enum.reduce(min_row..max_row, {nil, %{}}, fn
        row, {nil, acc} ->
          case Map.get(map, row) do
            nil ->
              {nil, acc}

            {s, e} ->
              a = {s, e} |> Tuple.to_list() |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, row, a)}
          end

        row, {{xx, yy} = curr, acc} ->
          case Map.get(map, row) do
            nil ->
              {curr, Map.put(acc, row, curr)}

            {s, e} ->
              [ss, ee] =
                {s, e}
                |> Tuple.to_list()
                |> Enum.sort()

              a = [min(xx, ss), max(yy, ee)] |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, row, a)}
          end
      end)
      |> IO.inspect(label: "Top to Bottom")

    {_, bottom_to_top} =
      Enum.reduce(max_row..min_row//-1, {nil, %{}}, fn
        row, {nil, acc} ->
          case Map.get(map, row) do
            nil ->
              {nil, acc}

            {s, e} ->
              a = {s, e} |> Tuple.to_list() |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, row, a)}
          end

        row, {{xx, yy} = curr, acc} ->
          case Map.get(map, row) do
            nil ->
              {curr, Map.put(acc, row, curr)}

            {s, e} ->
              [ss, ee] =
                {s, e}
                |> Tuple.to_list()
                |> Enum.sort()

              a = [min(xx, ss), max(yy, ee)] |> Enum.sort() |> List.to_tuple()
              {a, Map.put(acc, row, a)}
          end
      end)
      |> IO.inspect(label: "Bottom to Top")

    recursive_two(reds, map, [], top_to_bottom, bottom_to_top, left_to_right, right_to_left)
    |> Enum.max()
    |> dbg()
  end

  def recursive_two([_], _map, result, _, _, _, _), do: result

  def recursive_two([{a, b} | tail], map, result, tb, bt, lr, rl) do
    max =
      for {x, y} <- tail do
        if check_green({a, y}, {x, b}, tb, bt, lr, rl) do
          (abs(a - x) + 1) * (abs(b - y) + 1)
        else
          0
        end
      end
      |> Enum.max()

    recursive_two(tail, map, [max | result], tb, bt, lr, rl)
  end

  defp check_green({a, b}, {c, d}, tb, bt, lr, rl) do
    one =
      case {tb[b], bt[b]} do
        {{q, w}, {e, r}} ->
          if q <= a and w >= a and e <= a and r >= a do
            true
          else
            false
          end

        _ ->
          false
      end

    two =
      case {tb[d], bt[d]} do
        {{q, w}, {e, r}} ->
          if q <= c and w >= c and e <= c and r >= c do
            true
          else
            false
          end

        _ ->
          false
      end

    three =
      case {lr[a], rl[a]} do
        {{q, w}, {e, r}} ->
          if q <= b and w >= b and e <= b and r >= b do
            true
          else
            false
          end

        _ ->
          false
      end

    four =
      case {lr[c], rl[c]} do
        {{q, w}, {e, r}} ->
          if q <= d and w >= d and e <= d and r >= d do
            true
          else
            false
          end

        _ ->
          false
      end

    one and two and three and four
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
Solution.part_one(file_path)
Solution.part_two(file_path)

defmodule Day13.PartOne do
  def run([_], sum), do: sum

  def run([{[left, right], index} | tail], sum) do
    case do_compare(left, right) do
      true ->
        run(tail, sum + index)

      _ ->
        run(tail, sum)
    end
  end

  def do_compare(left, right) when left == right, do: :equal

  def do_compare([], _right), do: true

  def do_compare(_left, []), do: false

  def do_compare([l_h | l_tail], [r_h | r_tail])
      when is_integer(l_h) and
             is_integer(r_h) do
    case l_h - r_h do
      0 ->
        do_compare(l_tail, r_tail)

      result when result < 0 ->
        true

      _ ->
        false
    end
  end

  def do_compare([l_h | l_tail], [r_h | r_tail])
      when is_list(l_h) and is_list(r_h) do
    case do_compare(l_h, r_h) do
      :equal -> do_compare(l_tail, r_tail)
      true -> true
      _ -> false
    end
  end

  def do_compare([l_h | _] = left, [r_h | r_tail]) when is_list(l_h) do
    do_compare(left, [[r_h] | r_tail])
  end

  def do_compare([l_h | l_tail], [r_h | _] = right) when is_list(r_h) do
    do_compare([[l_h] | l_tail], right)
  end
end

File.read!("./day_13/input.txt")
|> String.split("\n\n")
|> Enum.map(fn str ->
  str
  |> String.split("\n")
  |> Enum.map(fn inner_str ->
    {result, _binging} = Code.eval_string(inner_str)
    result
  end)
end)
|> Enum.with_index(1)
|> Day13.PartOne.run(0)
|> IO.inspect()

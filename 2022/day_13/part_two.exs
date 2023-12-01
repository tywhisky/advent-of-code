defmodule Day13.PartTwo do
  def run(packets) do
    (packets ++ [[[2]], [[6]]])
    |> Enum.sort(fn a, b -> do_compare(a, b) in [true, :equal] end)
    |> Enum.with_index(1)
    |> Enum.filter(&(elem(&1, 0) in [[[2]], [[6]]]))
    |> Enum.map(&elem(&1, 1))
    |> Enum.product()
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
|> Enum.flat_map(&String.split(&1, "\n"))
|> Enum.map(fn str ->
  {result, _binding} = Code.eval_string(str)
  result && result
end)
|> Enum.reject(&is_nil(&1))
|> Day13.PartTwo.run()
|> IO.inspect()

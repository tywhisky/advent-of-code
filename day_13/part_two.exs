defmodule Day13.PartTwo do
  def run(packets) do
    (packets ++ [[[2]], [[6]]])
    |> Enum.sort_by(&until_get_integer/1, :asc)
    |> Enum.with_index(1)
    |> Enum.filter(&(elem(&1, 0) in [[[2]], [[6]]]))
    |> Enum.map(&elem(&1, 1))
    |> Enum.product()
    |> dbg()
  end

  def until_get_integer([]), do: 0
  def until_get_integer([[]]), do: 1
  def until_get_integer([[[]]]), do: 2

  def until_get_integer([h | tail]) when is_integer(h) do
    h
  end

  def until_get_integer([h | tail]) do
    until_get_integer(h)
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

defmodule Day09.PartTwo do
  def run([], _, record), do: record

  def run(["R" | tail], [{h_x, h_y} | c_tail], record) do
    chain = iter_follow([{h_x + 1, h_y} | c_tail], [])
    run(tail, chain, MapSet.put(record, List.last(chain)))
  end

  def run(["D" | tail], [{h_x, h_y} | c_tail], record) do
    chain = iter_follow([{h_x, h_y - 1} | c_tail], [])
    run(tail, chain, MapSet.put(record, List.last(chain)))
  end

  def run(["L" | tail], [{h_x, h_y} | c_tail], record) do
    chain = iter_follow([{h_x - 1, h_y} | c_tail], [])
    run(tail, chain, MapSet.put(record, List.last(chain)))
  end

  def run(["U" | tail], [{h_x, h_y} | c_tail], record) do
    chain = iter_follow([{h_x, h_y + 1} | c_tail], [])
    run(tail, chain, MapSet.put(record, List.last(chain)))
  end

  def iter_follow([last], result) do
    [last | result]
    |> Enum.reverse()
  end

  def iter_follow([{h_x, h_y} = h, {t_x, t_y} | tail], result) do
    new = follow(t_x, t_y, h_x, h_y)
    iter_follow([new | tail], [h | result])
  end

  def follow(t_x, t_y, h_x, h_y)
      when abs(h_x - t_x) <= 1 and
             abs(h_y - t_y) <= 1 do
    {t_x, t_y}
  end

  def follow(t_x, t_y, h_x, h_y) do
    {compute(h_x - t_x) + t_x, compute(h_y - t_y) + t_y}
  end

  defp compute(0), do: 0
  defp compute(n), do: n |> abs() |> div(n)
end

File.read!("./day_09/input.txt")
|> String.split("\n")
|> Enum.map(&String.split(&1, " "))
|> Enum.flat_map(fn [action, step] ->
    List.duplicate(action, String.to_integer(step))
end)
|> Day09.PartTwo.run(List.duplicate({0, 0}, 10), MapSet.new([{0, 0}]))
|> MapSet.to_list()
|> length()
|> IO.inspect()

defmodule Day08.PartOne do
  def run(input) do
    input
    |> from_left()
    |> from_top()
    |> from_right()
    |> from_bottom()
    |> List.flatten()
    |> Enum.count(&(elem(&1, 1) == true))
  end

  def run([], rest), do: Enum.reverse(rest)

  def run([h | tail], rest) do
    run(tail, [do_view(h, nil, []) | rest])
  end

  def do_view([], _, rest), do: Enum.reverse(rest)

  def do_view([{num, _checked?} | tail], nil, rest) do
    do_view(tail, num, [{num, true} | rest])
  end

  def do_view([{num, _checked?} | tail], max, rest) when num > max do
    do_view(tail, num, [{num, true} | rest])
  end

  def do_view([h | tail], max, rest) do
    do_view(tail, max, [h | rest])
  end

  def from_left(input), do: run(input, [])

  def from_top(input) do
    input
    |> Enum.zip_with(& &1)
    |> run([])
  end

  def from_right(input) do
    input
    |> Enum.map(&Enum.reverse(&1))
    |> run([])
  end

  def from_bottom(input) do
    input
    |> Enum.zip_with(& &1)
    |> Enum.map(&Enum.reverse(&1))
    |> run([])
  end
end

File.read!("./day_08/input.txt")
|> String.split("\r\n")
|> Enum.map(fn list ->
  list
  |> String.graphemes()
  |> Enum.map(fn l -> {l, false} end)
end)
|> Day08.PartOne.run()
|> IO.inspect()

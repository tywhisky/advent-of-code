defmodule Day21.PartTwo do
  def run(%{"humn" => %{val: humn_val}}) when is_integer(humn_val) do
    humn_val
  end

  def run(map) do
    new_map =
      map
      |> Enum.reduce(map, fn
        {curr, %{val: nil, express: express}}, acc ->
          case recursive_compute(express, acc) do
            {:ok, val} -> put_in(acc, [curr, :val], val)
            :error -> acc
          end

        _, acc ->
          acc
      end)

    run(new_map)
  end

  def recursive_compute([], _acc), do: :error

  def recursive_compute([h | tail], acc) do
    [a, op, b] = String.split(h, " ")

    if acc[a][:val] && acc[b][:val] do
      {:ok, do_compute(acc[a][:val], op, acc[b][:val])}
    else
      recursive_compute(tail, acc)
    end
  end

  def do_compute(a, "+", b), do: a + b
  def do_compute(a, "-", b), do: a - b
  def do_compute(a, "*", b), do: a * b
  def do_compute(a, "/", b), do: div(a, b)
end

"#{__DIR__}/input.txt"
|> File.read!()
|> String.split("\n")
|> Enum.map(&String.split(&1, ": "))
|> Enum.flat_map(fn
  ["root", value] ->
    [a, _, b] = String.split(value)

    [
      {"root", 0},
      {a, ["root", "+", b] |> Enum.join(" ")},
      {b, [a, "-", "root"] |> Enum.join(" ")}
    ]

  ["humn", _value] ->
    []

  [key, value] ->
    if String.length(value) < 11 do
      [{key, String.to_integer(value)}]
    else
      [a, op, b] = String.split(value, " ")

      case op do
        "+" ->
          [{key, value}, {a, "#{key} - #{b}"}, {b, "#{key} - #{a}"}]

        "-" ->
          [{key, value}, {a, "#{key} + #{b}"}, {b, "#{a} - #{key}"}]

        "*" ->
          [{key, value}, {a, "#{key} / #{b}"}, {b, "#{key} / #{a}"}]

        "/" ->
          [{key, value}, {a, "#{key} * #{b}"}, {b, "#{a} / #{key}"}]
      end
    end
end)
|> Enum.group_by(&elem(&1, 0), &elem(&1, 1))
|> Enum.map(fn {key, value} ->
  case Enum.find(value, &is_integer/1) do
    nil -> {key, %{val: nil, express: value}}
    val -> {key, %{val: val, express: List.delete(value, val)}}
  end
end)
|> Map.new()
|> Day21.PartTwo.run()
|> IO.inspect()

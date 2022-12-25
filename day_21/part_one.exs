defmodule Day21.PartOne do
  def run(%{"root" => root}) when is_integer(root) do
    root
  end

  def run(map) do
    new_map =
      map
      |> Enum.reduce(map, fn {key, value}, acc ->
        if is_integer(value) do
          acc
        else
          new_value = compute(String.split(value, " "), acc)
          Map.put(acc, key, new_value)
        end
      end)

    run(new_map)
  end

  def compute([a, operation, b] = value, map) do
    if is_integer(map[a]) and is_integer(map[b]) do
      do_compute(map[a], operation, map[b])
    else
      Enum.join(value, " ")
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
|> Enum.map(fn [key, value] ->
  value = if String.length(value) < 11, do: String.to_integer(value), else: value
  {key, value}
end)
|> Map.new()
|> Day21.PartOne.run()
|> IO.inspect()

defmodule Day06 do
  def part_one() do
    parse()
    |> Enum.map(fn [time, distance] ->
      0..time
      |> Enum.map(fn hold_speed ->
        (time - hold_speed) * hold_speed
      end)
      |> Enum.reject(&(&1 <= distance))
      |> Enum.count()
    end)
    |> Enum.product()
  end

  def part_two() do
    {time, distance} =
      parse()
      |> Enum.reduce({"", ""}, fn [t, d], {time, distance} ->
        {"#{time}#{t}", "#{distance}#{d}"}
      end)
      |> then(fn {t, d} -> {String.to_integer(t), String.to_integer(d)} end)

    head = find_edge(Enum.to_list(0..time), time, distance)
    last = find_edge(Enum.to_list(time..0), time, distance)

    last - head + 1
  end

  def find_edge([h | _tail], time, distance) when (time - h) * h > distance do
    h
  end

  def find_edge([_h | tail], time, distance) do
    find_edge(tail, time, distance)
  end

  def parse() do
    "input.txt"
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.map(&tl/1)
    |> Enum.zip_with(fn [x, y] -> [String.to_integer(x), String.to_integer(y)] end)
  end
end

IO.inspect("The Result of Day06 Part One is: #{Day06.part_one()}")
IO.inspect("The Result of Day06 Part Two is: #{Day06.part_two()}")

defmodule Day09.PartOne do
  def run([], _, record), do: record
  def run([direction | tail], {0, 0, 0, 0}, record) do
    case direction do
        "R" -> run(tail, {0, 0, 1, 0}, record)
        "L" -> run(tail, {0, 0, -1, 0}, record)
        "U" -> run(tail, {0, 0, 0, 1}, record)
        "D" -> run(tail, {0, 0, 0, -1}, record)
    end
  end
  # 在同一行，y 轴相同
  def run(["R" | tail], {t_x, y, h_x, y}, record) do
    run(tail, {t_x + 1, y, h_x + 1, y}, MapSet.put(record, {t_x + 1, y}))
  end

  # H 在右上角
  def run(["R" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y - 1 == t_y do
    run(tail, {t_x + 1, t_y + 1, h_x + 1, h_y}, MapSet.put(record, {t_x + 1, t_y + 1}))
  end

  # H 在左下角
  def run(["R" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y + 1 == t_y do
    run(tail, {t_x + 1, t_y - 1, h_x + 1, h_y}, MapSet.put(record, {t_x + 1, t_y - 1}))
  end

  def run(["R" | tail], {t_x, t_y, h_x, h_y}, record) do
    run(tail, {t_x, t_y, h_x + 1, h_y}, record)
  end

  # 在同一列, x 轴相同
  def run(["D" | tail], {x, t_y, x, h_y}, record) do
    run(tail, {x, t_y + 1, x, h_y + 1}, MapSet.put(record, {x, t_y + 1}))
  end

  # H 在左下角
  def run(["D" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y + 1 == t_y do
    run(tail, {t_x - 1, t_y - 1, h_x, h_y - 1}, MapSet.put(record, {t_x - 1, t_y - 1}))
  end

  # H 在右下角
  def run(["D" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y + 1 == t_y do
    run(tail, {t_x + 1, t_y - 1, h_x, h_y - 1}, MapSet.put(record, {t_x + 1, t_y - 1}))
  end

  def run(["D" | tail], {t_x, t_y, h_x, h_y}, record) do
    run(tail, {t_x, t_y, h_x, h_y - 1}, record)
  end

  # 在同一行， y轴相同
  def run(["L" | tail], {t_x, y, h_x, y}, record) do
    run(tail, {t_x - 1, y, h_x - 1, y}, MapSet.put(record, {t_x - 1, y}))
  end

  # H 在左上角
  def run(["L" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x + 1 == t_x and h_y - 1 == t_y do
    run(tail, {t_x - 1, t_y + 1, h_x - 1, h_y}, MapSet.put(record, {t_x - 1, t_y + 1}))
  end

  # H 在左下角
  def run(["L" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y + 1 == t_y do
    run(tail, {t_x - 1, t_y - 1, h_x - 1, h_y}, MapSet.put(record, {t_x - 1, t_y - 1}))
  end

  def run(["L" | tail], {t_x, t_y, h_x, h_y}, record) do
    run(tail, {t_x, t_y, h_x - 1, h_y}, record)
  end

  # 在同一列，x 轴相同
  def run(["U" | tail], {x, t_y, x, h_y}, record) do
    run(tail, {x, t_y + 1, x, h_y + 1}, MapSet.put(record, {x, t_y + 1}))
  end

  # H 在左上角
  def run(["U" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x + 1 == t_x and h_y - 1 == t_y do
    run(tail, {t_x - 1, t_y + 1, h_x, h_y + 1}, MapSet.put(record, {t_x - 1, t_y + 1}))
  end

  # H 在右上角
  def run(["U" | tail], {t_x, t_y, h_x, h_y}, record)
      when h_x - 1 == t_x and h_y - 1 == t_y do
    run(tail, {t_x + 1, t_y + 1, h_x, h_y + 1}, MapSet.put(record, {t_x + 1, t_y + 1}))
  end

  def run(["U" | tail], {t_x, t_y, h_x, h_y}, record) do
    run(tail, {t_x, t_y, h_x, t_y + 1}, record)
  end
end

File.read!("./day_09/input.txt")
|> String.split("\n")
|> Enum.map(&String.split(&1, " "))
|> Enum.flat_map(fn [action, step] ->
  List.duplicate(action, String.to_integer(step))
end)
|> IO.inspect
|> Day09.PartOne.run({0, 0, 0, 0}, MapSet.new([{0, 0}]))
|> IO.inspect()
|> MapSet.to_list()
|> length()
|> IO.inspect()
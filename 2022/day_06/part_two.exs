defmodule Day06.PartTwo do
  def run(_list, record) when length(record) >= 14 do
    {_value, result} = Enum.max_by(record, &elem(&1, 1))
    result
  end

  def run([{value, index} | tail], record) do
    got? = Enum.filter(record, &(elem(&1, 0) == value))
    if got? == [] do
      run(tail, [{value, index} | record])
    else
      max_record_index = Enum.max_by(got?, &elem(&1, 1)) |> elem(1)
      new_record = Enum.filter(record, &(elem(&1, 1) > max_record_index))
      run(tail, [{value, index} | new_record])
    end
  end
end

File.read!("./day_06/input.txt")
|> String.replace("\n", "")
|> String.graphemes()
|> Enum.with_index(1)
|> Day06.PartTwo.run([])
|> IO.inspect()

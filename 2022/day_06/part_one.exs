defmodule Day06.PartOne do
  def run(_list, record) when length(record) >= 4 do
    {_value, result} = Enum.max_by(record, &elem(&1, 1))
    result
  end

  def run([{value, index} | tail], record) do
    if Enum.find(record, &(elem(&1, 0) == value)) == nil do
      run(tail, [{value, index} | record])
    else
      new_record = Enum.filter(record, &(elem(&1, 1) > index))
      run(tail, [{value, index} | new_record])
    end
  end
end

File.read!("./day_06/input.txt")
|> String.replace("\n", "")
|> String.graphemes()
|> Enum.with_index(1)
|> Day06.PartOne.run([])
|> IO.inspect()

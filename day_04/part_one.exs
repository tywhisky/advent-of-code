File.read!("./day_04/input.txt")
|> String.split("\n")
|> Enum.reject(& &1 == "")
|> Enum.map(fn str ->
  [first, second] = String.split(str, ",")
  [f_1, f_2] = String.split(first, "-") |> Enum.map(&String.to_integer(&1))
  [s_1, s_2] = String.split(second, "-") |> Enum.map(&String.to_integer(&1))

  if (f_1 <= s_1 and f_2 >= s_2) or (f_1 >= s_1 and f_2 <= s_2), do: 1, else: 0
end)
|> Enum.sum()
|> IO.inspect()

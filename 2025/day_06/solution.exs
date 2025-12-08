defmodule Solution do
  def parse(input) do
    input
    |> File.read!()
    |> String.split("\n", trim: true)
    |> Enum.map(&String.split(&1, " ", trim: true))
    |> Enum.reverse()
  end

  def part_one(input) do
    [actions | nums] =
      parse(input)

    nums
    |> Enum.reverse()
    |> Enum.zip()
    |> Enum.map(&Tuple.to_list/1)
    |> Enum.reduce({actions, 0}, fn num_list, {[action | rest_actions], result} ->
      case action do
        "+" ->
          acc =
            num_list
            |> Enum.map(&String.to_integer/1)
            |> Enum.sum()

          {rest_actions, result + acc}

        "*" ->
          acc =
            num_list
            |> Enum.map(&String.to_integer/1)
            |> Enum.product()

          {rest_actions, result + acc}
      end
    end)
    |> IO.inspect(label: "Part One Result")
  end

  def part_two(input) do
    [actions | nums] =
      input
      |> File.read!()
      |> String.split("\n", trim: true)
      |> Enum.reverse()

    actions =
      actions
      |> String.split(" ", trim: true)

    nums
    |> Enum.reverse()
    |> Enum.map(&String.graphemes/1)
    |> Enum.zip()
    |> Enum.map(fn tuple ->
      tuple
      |> Tuple.to_list()
      |> Enum.join()
      |> String.trim()
    end)
    |> Enum.concat([""])
    |> Enum.reduce(
      {actions, [], 0},
      fn
        "", {[action | rest_actions], group, result} ->
          case action do
            "+" ->
              acc =
                group
                |> Enum.map(&String.to_integer/1)
                |> Enum.sum()

              {rest_actions, [], result + acc}

            "*" ->
              acc =
                group
                |> Enum.map(&String.to_integer/1)
                |> Enum.product()

              {rest_actions, [], result + acc}
          end

        num_str, {as, group, result} ->
          {as, [num_str | group], result}
      end
    )
    |> IO.inspect(label: "Part Two Result")
  end
end

file_path = if System.argv() == [], do: "test.txt", else: "input.txt"
Solution.part_one(file_path)
Solution.part_two(file_path)

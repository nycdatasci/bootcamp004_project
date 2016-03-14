defmodule ElyanahExample.CLI do
  require Record
  Record.defrecord :xmlElement, Record.extract(:xmlElement, from_lib: "xmerl/include/xmerl.hrl")
  Record.defrecord :xmlText,    Record.extract(:xmlText,    from_lib: "xmerl/include/xmerl.hrl")
  Record.defrecord :xmlAttribute, Record.extract(:xmlAttribute, from_lib: "xmerl/include/xmerl.hrl")


  def run(argv) do
    parse_args(argv)
    |> process
  end

  def parse_args(argv) do
    parse = OptionParser.parse(argv)
    case parse do
      {_, [command, max, eta | topology], _} -> {command, max, eta, topology}
    end
  end

  def process({command, max, eta, topology}) do
    {:ok, net} = GenServer.start_link(Elyanah.NeuralNet, topology)
    {:ok, stats} = Agent.start_link(fn -> %{count: 0, errors: 0, exp_avg: 1} end)
    get_stream(command)
    |> Stream.map(&(train(&1, net, stats, eta)))
    |> Stream.map(&(IO.inspect(Map.take(&1, [:exp_avg, :pred]))))
    |> Enum.take(max)
    IO.inspect Agent.get(stats, &(&1))
  end

  def get_stream("shots") do
    "data/game_ids"
    |> File.stream!
    |> Stream.map(&download_game/1)
    |> Stream.filter(&(&1 != :fail))
    |> Stream.flat_map(&parse_game/1)
    |> Stream.cycle
  end

  def get_stream("simple") do
    [{[0,1], 1}, {[1, 0], 0}]
    |> Stream.cycle
  end

  def get_stream("words") do
    "data/words"
    |> File.stream!
    |> Enum.map(fn word ->
                    {String.split(word, ""), rem(String.length(word), 2)}
                  end)
    |> Stream.cycle
  end

  def get_stream("random") do
    zero = Stream.repeatedly(fn -> :random.uniform end) |> Enum.take(10)
    one = Stream.repeatedly(fn -> :random.uniform end) |> Enum.take(10)
    dists = %{0 => zero, 1 => one}
    Stream.repeatedly(
        fn ->
          out = :random.uniform(2) - 1
          base = dists[out]
          {Enum.map(dists[out], fn x -> x + :random.uniform end), out}
        end
    )
  end

  def get_stream(command) do
    IO.inspect command
  end

  def train({input, made}, net, stats, eta) do
    [prediction] = GenServer.call(net, {:predict, input})
    GenServer.call(net, {:backprop, [made], eta})
    err = made - prediction
    sq_err = err * err
    {errors, count, exp_avg} =
        Agent.get_and_update(
            stats,
            fn state ->
              %{count: count, errors: errors, exp_avg: exp_avg} = state
              count = 1 + count
              errors = errors + sq_err
              exp_avg = 0.01 * sq_err + 0.99 * exp_avg
              {{errors, count, exp_avg}, %{count: count, errors: errors, exp_avg: exp_avg}}
            end
        )
    %{pred: prediction, actual: made, error: err, sq_err: sq_err, avg: errors/count, exp_avg: exp_avg}
  end

  def download_game(game_id) do
    game_url(game_id)
    |> HTTPoison.get
    |> handle_response
  end

  def handle_response({:ok, %{status_code: 200, body: body}}) do
    body
  end

  def handle_response(response) do
    IO.inspect(response)
    :fail
  end

  def parse_game(game_text) do
    game_text
    |> String.to_char_list
    |> :xmerl_scan.string
    |> parse_xml
  end

  def parse_xml({ xml, _ }) do
    # multiple elements
    :xmerl_xpath.string('/Shots//Shot', xml)
    |> Stream.map(
          fn(element) ->
            pid = attr(element, "pid")
            qtr = attr(element, "qtr") |> String.to_integer
            x = attr(element, "x") |> String.to_integer
            y = attr(element, "y") |> String.to_integer
            t = attr(element, "t")
            {xx, yy} = transform_coords(x,y,t)
            dist = :math.sqrt(xx * xx + yy * yy)
            rads = calc_rads(xx, yy)
            made =
                case attr(element, "made") do
                  "false" -> 0
                  "true" -> 1
                end
            {[qtr, x, y, t, xx, yy, dist, rads, pid], made}
          end
    )
  end

  def transform_coords(x,y,"a"), do: {x - 25, y - 4}
  def transform_coords(x,y,"h"), do: {25 - x, 90 - y}
  def calc_rads(0, y) when y > 0, do: 0
  def calc_rads(0, _), do: :math.pi
  def calc_rads(x, 0) when x > 0, do: :math.pi / 2
  def calc_rads(_, 0), do: -:math.pi / 2
  def calc_rads(x, y) when y > 0, do: :math.atan(x / y)
  def calc_rads(x, y), do: :math.atan(x / y) - :math.pi

  def game_url(game_id) do
    url = "http://espn.go.com/nba/gamepackage/data/shot?gameId=#{String.strip(game_id)}"
    IO.inspect url
    url
  end

  def attr(node, name), do: node |> xpath('./@#{name}') |> extract_attr
  defp extract_attr([xmlAttribute(value: value)]), do: List.to_string(value)
  defp extract_attr(_), do: nil

  defp xpath(nil, _), do: []
  defp xpath(node, path), do: :xmerl_xpath.string(to_char_list(path), node)

end

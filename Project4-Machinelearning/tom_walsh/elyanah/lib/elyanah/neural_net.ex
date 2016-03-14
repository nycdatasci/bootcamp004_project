defmodule Elyanah.NeuralNet do
  use GenServer

  defmodule State do
    defstruct nodes: [], prediction: nil, column_map: nil
  end

  def init(hierarchy) do
    nodes = hierarchy |> Enum.map(&(build_layer/1))
    {:ok, column_map} = Agent.start_link(fn -> %{} end)
    {:ok, %State{nodes: nodes, column_map: column_map}}
  end

  def handle_call({:predict, inputs}, _from, state) do
    inputs = inputs |> transform_inputs(state.column_map)
    prediction =
      Enum.reduce(state.nodes, inputs, &(predict_layer/2))
    {:reply, prediction, %{state | prediction: prediction}}
  end

  def handle_call({:backprop, targets, eta}, _from, state) do
    outs = state.prediction
    deltas = Enum.zip(targets, outs)
             |> Enum.map(fn {target, out} -> (out - target) * out * (1 - out) end)
    Enum.reverse(state.nodes)
    |> Enum.with_index
    |> Enum.reduce(%{deltas: deltas, eta: eta, n_weights: [], n_deltas: []}, &backprop_layer/2)
    {:reply, :ok, state}
  end

  def build_layer(num_nodes) do
    Stream.repeatedly(&(build_node/0)) |> Enum.take(num_nodes)
  end

  def build_node do
    {:ok, pid} = Agent.start_link(fn ->
                                    :random.seed(:os.timestamp)
                                    %{weights: [], prediction: nil, last_input: nil}
                                  end)
    pid
  end

  def predict_layer(layer, inputs) do
    layer
    |> Stream.map(&(Task.async(fn -> predict_node(&1, [1|inputs]) end)))
    |> Enum.map(&(Task.await/1))
  end

  def backprop_layer({layer, l_num}, calcs) do
    %{eta: eta, n_weights: weights, n_deltas: deltas} =
        layer
        |> Stream.with_index
        |> Stream.map(&(Task.async(fn -> backprop_node(&1, l_num, calcs) end)))
        |> Stream.map(&(Task.await/1))
        |> Enum.reduce(
              calcs,
              fn x, acc ->
                %{n_weights: pw_acc, n_deltas: nd_acc} = acc
                %{n_weights: pw_x, n_deltas: nd_x} = x
                %{calcs | n_weights: pw_acc ++ pw_x, n_deltas: nd_acc ++ nd_x}
              end)
    %{deltas: deltas, weights: weights, eta: eta, n_weights: [], n_deltas: []}
  end

  def backprop_node({node, n_num}, l_num, calcs) do
    Agent.get_and_update(
        node,
        fn state ->
          %{weights: n_weights} = state
          {weights, calcs} = backprop_weights(calcs, state, l_num, n_num)
          {%{calcs | n_weights: [n_weights]}, %{state | weights: weights}}
        end
    )
  end

  def backprop_weights(calcs, state, 0, n_num) do
    %{deltas: deltas, eta: eta, n_deltas: n_deltas} = calcs
    %{last_input: inputs, weights: weights} = state
    {:ok, delta} = Enum.fetch(deltas, n_num)
    dEdw_l = inputs |> Enum.map(&(&1 * delta))
    weights = Enum.zip(weights, dEdw_l)
              |> Enum.map(fn {w, dEdw} -> w - eta * dEdw end)
    {weights, %{calcs | n_deltas: [delta]}}
  end

  def backprop_weights(calcs, state, l_num, n_num) do
    %{deltas: deltas, eta: eta, weights: n_weights} = calcs
    %{last_input: inputs, weights: weights, prediction: out} = state
    delta_sum = deltas
                |> Enum.with_index
                |> Enum.map(
                      fn {delta, index} ->
                        {:ok, ww} = Enum.fetch(n_weights, index)
                        {:ok, w} = Enum.fetch(ww, 1 + n_num)
                        delta * w
                      end)
                |> Enum.sum
    n_delta = delta_sum * out * (1 - out)
    dEdw_l = inputs |> Enum.map(&(&1 * n_delta))
    weights = Enum.zip(weights, dEdw_l)
              |> Enum.map(fn {w, dEdw} -> w - eta * dEdw end)
    {weights, %{calcs | n_deltas: [n_delta]}}
  end

  def predict_node(node, inputs) do
    Agent.get_and_update(
        node,
        fn state ->
          %{weights: weights} = state
          weights = add_weights(weights, inputs)
          prediction = predict(weights, inputs)
          {prediction, %{state | weights: weights, prediction: prediction, last_input: inputs}}
        end
    )
  end

  def predict(weights, inputs), do: dot(weights, inputs) |> sigmoid

  def sigmoid(z) when z < -709, do: sigmoid(-709)
  def sigmoid(z), do: 1 / (1 + :math.exp( -z ))

  def dot(x,y), do: _dot(x,y,0)
  defp _dot([], _, acc), do: acc
  defp _dot(_, [], acc), do: acc
  defp _dot([x|xx], [y|yy], acc), do: _dot(xx, yy, acc + x * y)

  def add_weights(weights, inputs) when length(weights) >= length(inputs) do
    weights
  end
  def add_weights(weights, inputs) do
    _add_weights(weights,inputs, [])
    |> Enum.reverse
  end
  defp _add_weights([], [], acc), do: acc
  defp _add_weights([w|ww], [_|ii], acc), do: _add_weights(ww,ii,[w|acc])
  defp _add_weights([], [i|ii], acc) do
    w = (:random.uniform - 0.5) / Enum.max([i, 0.1])
    _add_weights([], ii, [w|acc])
  end

  def transform_inputs(inputs, column_map) do
    case inputs |> Enum.group_by(&is_number/1) do
      %{false: factors, true: inputs} ->
        Enum.reverse(inputs) ++ transform_factors(factors, column_map)
      %{true: inputs} ->
        Enum.reverse(inputs)
      %{false: factors} ->
        transform_factors(factors, column_map)
    end
  end

  def transform_factors(factors, column_map) do
    factors = factors
              |> Enum.with_index
              |> Enum.reduce(
                    %{},
                    fn key, map ->
                      index = Agent.get_and_update(
                          column_map,
                          fn state ->
                            state = Map.put_new(state, key, Enum.count(state))
                            {state[key], state}
                          end
                      )
                      Map.put(map, index, 1)
                    end)
    0..Enum.max(Map.keys(factors))
    |> Enum.map(&(Map.get(factors, &1, 0)))
  end

end

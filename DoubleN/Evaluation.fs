module DoubleN.Evaluation

open DoubleN.DomainTypes
open DoubleN.Initialization

let evaluateNeuron (inputs:float seq) neuron =
    let value = inputs |> Seq.map2(*) neuron.weights |> Seq.reduce(+)
    match neuron.activation with
    | Sigmoid fn -> value |> fn
    | _ -> invalidArg "neuron" "Invalid activation function" |> raise

let evaluateLayer inputs layer =
    let bias = Seq.singleton 1.0
    let inputsWithBias = inputs |> Seq.append bias
    layer.neurons
    |> Seq.map(evaluateNeuron inputs)

let evaluateNetwork network inputs =
    network.layers
    |> Seq.fold(fun state layer -> layer |> evaluateLayer state) inputs

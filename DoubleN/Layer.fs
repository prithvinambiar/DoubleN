module DoubleN.Layer

open DoubleN.Neuron

type Layer = {
    neurons : Neuron seq
}

let initialize numberOfInputs numberOfNeurons =
    seq { 1..numberOfNeurons }
    |> Seq.map(fun _ -> numberOfInputs |> Neuron.initialize)
    |> fun neurons -> {neurons = neurons}
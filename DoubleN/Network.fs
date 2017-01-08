module DoubleN.Network

open DoubleN.DataStructure
open DoubleN.Layer
open DoubleN.Neuron
open System

type Network = {
   layers : Layer seq 
}


let initialize layerInfo numberOfInputs=
    layerInfo
    |> Seq.mapFold(fun state numberOfNeurons ->
            let layer = Layer.initialize numberOfInputs numberOfNeurons
            layer, numberOfNeurons
       ) numberOfInputs
    |> fun (layers, _) -> { layers = layers}


//let evaluateNeuron (input:float list) neuron =
//    let weights = neuron.inputs |> List.map(fun (_, Weight w) -> w)
//    let v = 1.0::input |> List.map2(*) weights |> List.reduce(+)
//    match neuron.activation with
//    | Sigmoid fn -> fn v
//    | _ -> invalidArg "neuron" "Invalid activation function" |> raise
//
//let evaluateLayer input layer =
//    layer.neurons
//    |> List.map(evaluateNeuron input)
//
//let evaluate network input =
//    List.fold(fun state layer -> layer |> evaluateLayer state) input network.layers

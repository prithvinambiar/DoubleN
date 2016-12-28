module DoubleN.Network

open DoubleN.DataStructure
open DoubleN.Layer
open DoubleN.Neuron
open System

let a = System.Random()

let sigmoidFn =
    Sigmoid (fun number -> 1.0 / (1.0+exp(-number)))

let createNeuron previousLayer numberOfInputs =
    match previousLayer with
    | Some layer ->
        let inputConnections =
            layer.neurons
            |> List.map(fun n -> Some n, a.NextDouble() |> Weight)
            |> List.append [None, a.NextDouble() |> Weight]
        {inputs = inputConnections; activation = sigmoidFn}
    | None ->
        let inputConnections =
            [1..numberOfInputs]
            |> List.map(fun _ -> None, a.NextDouble() |> Weight)
            |> List.append [None, a.NextDouble() |> Weight]
        {inputs = inputConnections; activation = sigmoidFn}

let createLayer numberOfNeurons previousLayer numberOfInputs =
    let neurons =
        [1..numberOfNeurons]
        |> List.map(fun _ -> createNeuron previousLayer numberOfInputs)
    {neurons = neurons}

let createNetwork layerInfos numberOfInputs=
    let layers =
        List.mapFold(fun state noOfNeurons ->
                    let layer = createLayer noOfNeurons state numberOfInputs
                    layer, layer |> Some
                ) None layerInfos
        |> fst
    {layers = layers}

let evaluateNeuron (input:float list) neuron =
    let weights = neuron.inputs |> List.map(fun (_, Weight w) -> w)
    let v = 1.0::input |> List.map2(*) weights |> List.reduce(+)
    match neuron.activation with
    | Sigmoid fn -> fn v
    | _ -> invalidArg "neuron" "Invalid activation function" |> raise

let evaluateLayer input layer =
    layer.neurons
    |> List.map(evaluateNeuron input)

let evaluate network input =
    List.fold(fun state layer -> layer |> evaluateLayer state) input network.layers

module DoubleN.Initialization

open DoubleN.DomainTypes
open DoubleN.RandomNumber
open Microsoft.FSharp.Collections

let a = System.Random()

let sigmoidFn =
    Sigmoid (fun number -> 1.0 / (1.0+exp(-number)))


let initializeNeuron numberOfInputs =
    seq {1 .. numberOfInputs}
    |> Seq.map(fun _ -> generateNumber())
    |> fun weights -> {weights = weights; activation = sigmoidFn}

let initializeLayer numberOfInputs numberOfNeurons =
    let numberOfInputsWithBias = numberOfInputs + 1
    seq { 1..numberOfNeurons }
    |> Seq.map(fun _ -> numberOfInputsWithBias |> initializeNeuron)
    |> fun neurons -> {neurons = neurons}

let initializeNetwork layerInfo =
    let numberOfInputs = Seq.head layerInfo
    layerInfo
    |> Seq.tail
    |> Seq.mapFold(fun numberOfInputs numberOfNeurons ->
            let layer = initializeLayer numberOfInputs numberOfNeurons
            layer, layer.neurons |> Seq.length
       ) numberOfInputs
    |> fun (layers, _) -> { layers = layers}

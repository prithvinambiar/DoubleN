module DoubleN.Initialization

open DoubleN.DomainTypes
open DoubleN.RandomNumber

let a = System.Random()

let sigmoidFn =
    Sigmoid (fun number -> 1.0 / (1.0+exp(-number)))


let initializeNeuron numberOfInputs =
    seq {1 .. numberOfInputs}
    |> Seq.map(fun _ -> generateNumber())
    |> fun weights -> {weights = weights; activation = sigmoidFn}

let initializeLayer numberOfInputs numberOfNeurons =
    let numberOfInputsWithBias = numberOfInputs + 1
    seq { 1..numberOfInputsWithBias }
    |> Seq.map(fun _ -> numberOfInputs |> initializeNeuron)
    |> fun neurons -> {neurons = neurons}

let initializeNetwork layerInfo numberOfInputs =
    layerInfo
    |> Seq.mapFold(fun state numberOfNeurons ->
            let layer = initializeLayer numberOfInputs numberOfNeurons
            layer, numberOfNeurons
       ) numberOfInputs
    |> fun (layers, _) -> { layers = layers}

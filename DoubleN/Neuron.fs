module DoubleN.Neuron

open DoubleN.RandomNumber

type Weight = Weight of float
type Bias = Bias of float
type Input = Input of float
type Activation = Sigmoid of (float -> float)

type Neuron = {
    inputs : (Input * Weight) seq
    activation : Activation
}

let a = System.Random()

let sigmoidFn =
    Sigmoid (fun number -> 1.0 / (1.0+exp(-number)))


let initialize numberOfInputs =
    seq {1 .. numberOfInputs}
    |> Seq.map(fun _ -> 0.0 |> Input , (generateNumber() |> Weight))
    |> fun inputs -> {inputs = inputs; activation = sigmoidFn}

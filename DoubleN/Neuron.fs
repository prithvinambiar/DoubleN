module DoubleN.Neuron

type Weight = Weight of float
type Bias = Bias of float
type Activation = Sigmoid of (float -> float)

type Neuron = {
    inputs : (Neuron option * Weight) list
    activation : Activation
}

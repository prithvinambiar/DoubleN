module DoubleN.DataStructure

type Weight = Weight of float
type Bias = Bias of float
type Activation = Sigmoid of (float -> float)

type Neuron = {
    inputs : (Neuron option * Weight) list
    activation : Activation
}

type Layer = {
    neurons : Neuron list
}

type Network = {
   layers : Layer list 
}

type createNetwork = int -> Network

type evaluate = float list -> float list

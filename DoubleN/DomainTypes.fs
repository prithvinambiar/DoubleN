module DoubleN.DomainTypes

//type Weight = Weight of float
//type Bias = Bias of float
//type Input = Input of float
type Activation = Sigmoid of (float -> float)

type Neuron = {
    weights : float seq
    activation : Activation
}

type Layer = {
    neurons : Neuron seq
}

type Network = {
   layers : Layer seq
}

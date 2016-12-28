module DoubleN.DataStructure

open DoubleN.Layer

type Network = {
   layers : Layer list 
}

type createNetwork = int -> Network

type evaluate = float list -> float list

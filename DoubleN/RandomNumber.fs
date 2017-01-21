module DoubleN.RandomNumber

let a = System.Random()

let generateNumber () = 
    a.NextDouble() * 2.0 - 1.0 // numbers between -1.0 to 1.0

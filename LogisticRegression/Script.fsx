// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#r @"C:\D\New_folder\personal\prithvi\DoubleN\DoubleN\bin\Debug\DoubleN.dll"

open DoubleN.Initialization
open DoubleN.Evaluation

let nn = initializeNetwork [2; 100; 2]
evaluateNetwork nn [0.0;0.0]

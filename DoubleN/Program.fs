// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open DoubleN.DomainTypes
open FSharp.Charting


[<EntryPoint>]
let main argv =
    //let n = {inputs = [{neuron = None; input = Input 9.8; weight = Weight 7.5}]; outputs = []}
    printfn "%A" argv

    let c = FSharp.Charting.Chart.Line([0;1;2;3;4])
    c.ShowChart() |> ignore

    let x = [0.05 .. 0.15 .. 3500.0]
    let i1 = [for i in x -> (i, 1.0/(1.0 + System.Math.Exp (-i)))]
    let i2 = [for i in x -> (i, System.Math.Log(i))]

//    Chart.Combine[
//        //Chart.Line(i1)
//        Chart.Line(i2)
//    ].ShowChart()
    

    System.Console.ReadLine()

    0 // return an integer exit code

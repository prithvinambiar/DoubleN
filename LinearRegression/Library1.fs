module DoubleN.LogisticRegression

open System    

let sigmoid xi theta =
    xi
    |> List.fold2(fun s a b -> s + a * b) 0.0 theta
    |> fun res -> 1.0 / (1.0 + Math.Exp -res)

let step x y theta j =
    y
    |> List.map2(fun xi yi -> ((sigmoid xi theta) - yi) * xi.[j]) x
    |> List.reduce(+)

let cost x y theta=
    y
    |> List.map2(fun xi yi -> yi * -Math.Log (sigmoid xi theta) + (1.0 - yi) * -Math.Log (1.0 - (sigmoid xi theta))) x
    |> List.reduce(+)
    |> fun a -> a / (float)x.Length

let updateTheta (x:float list list) y theta alpha =
    theta
    |> List.mapi(fun i t -> t - (alpha * (step x y theta i)))

let regress (x:float list list) y =
    let x = 
        x
        |> List.map(fun xi -> xi |> List.append([1.0]))
    let r = Random()
    let theta =
        x.[0]
        |> List.map(fun _ -> r.NextDouble())
    
    let prevCost = 0.0
    let threshold = 0.001

    [1..10000]
    |> List.fold(fun theta _ ->
            let theta = updateTheta x y theta 0.01
            let cost' = cost x y theta
            cost' |> printfn "The cost is %A" |> ignore
            theta
        ) theta

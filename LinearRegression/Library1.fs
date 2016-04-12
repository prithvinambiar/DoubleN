module DoubleN.LogisticRegression

open System    

let random = Random()

let sigmoid xi theta =
    xi
    |> List.fold2(fun s a b -> s + a * b) 0.0 theta
    |> fun res -> 1.0 / (1.0 + Math.Exp -res)

let step x y theta j =
    y
    |> List.map2(fun xi yi -> ((sigmoid xi theta) - yi) * xi.[j]) x
    |> List.reduce(+)

let updateTheta (x:float list list) y theta alpha =
    let step' = step x y theta
    theta
    |> List.mapi(fun i t -> t - alpha * (step' i))

let cost x y theta=
    y
    |> List.map2(fun xi yi -> yi * -Math.Log (sigmoid xi theta) + (1.0 - yi) * -Math.Log (1.0 - (sigmoid xi theta))) x
    |> List.reduce(+)
    |> fun a -> a / (float)x.Length

let isConverged (cost:float) (prevCost:float) =
    let threshold = 0.0001
    cost |> printfn "The cost is %A" |> ignore
    Math.Abs (cost - prevCost) < threshold

let rec run x y theta iter prevCost =
    let maxIteration = 1000
    match iter < maxIteration with
    | true ->
        let theta = updateTheta x y theta 0.01
        let cost' = cost x y theta
        match isConverged cost' prevCost with
        | true -> printfn "Logistic regression has converged, cheers!!. Number of iteration %A" iter |> ignore; theta
        | false -> run x y theta (iter + 1) cost'
    | false ->
        printfn "Logistic regression didn't converge, sad :-(. Consider increasing the number of max iteration."
        theta

let regress (x:float list list) y =
    let x = 
        x
        |> List.map(fun xi -> xi |> List.append([1.0]))
    let theta =
        x.[0]
        |> List.map(fun _ -> random.NextDouble())
    
    run x y theta 0 0.0
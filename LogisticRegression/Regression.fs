module DoubleN.LogisticRegression

open System

let random = Random()

let sigmoid xi theta =
    xi
    |> Array.fold2(fun s a b -> s + a * b) 0.0 theta
    |> fun res -> 1.0 / (1.0 + Math.Exp -res)

let step x (y:float []) theta j =
    let temp = fun i xi ->((sigmoid xi theta) - y.[i]) * xi.[j]

    x
    |> Array.Parallel.mapi(temp)
    |> Array.reduce(+)

let updateTheta (x:float [] []) y theta alpha =
    let step' = step x y theta
    let temp = fun i t -> t - alpha * (step' i)

    theta
    |> Array.Parallel.mapi(temp)

let log num =
    match num <= 0.0 with
    | true -> 1000.0
    | false -> Math.Log num

let cost x (y:float []) theta=
    x
    |> Array.Parallel.mapi(fun i xi ->
        let temp =
            match sigmoid xi theta with
            | 0.0 -> 0.01
            | _ as a  -> a
        y.[i] * -log (temp) + (1.0 - y.[i]) * -log (1.0 - temp))
    |> Array.reduce(+)
    |> fun a -> a / (float)(x |> Array.length)

let isConverged (cost:float) (prevCost:float) =
    let threshold = 0.0001
    cost |> printfn "The cost is %A" |> ignore
    Math.Abs (cost - prevCost) < threshold

let rec run x y theta iter prevCost =
    let maxIteration = 5000
    match iter < maxIteration with
    | true ->
        printfn "Iteration %A started" iter |> ignore
        let theta = updateTheta x y theta 0.03
        let cost' = cost x y theta
        printfn "The cost is %A" cost'
        match false with
        | true -> printfn "Logistic regression has converged, cheers!!. Number of iteration %A" iter |> ignore; theta
        | false -> run x y theta (iter + 1) cost'
    | false ->
        printfn "Logistic regression didn't converge, sad :-(. Re-consider the alpha value and max number of iterations."
        printfn "Logistic regression theta %A" (theta |> Array.item 0)
        theta

let regress (x:float [] []) y =
    let x = 
        x
        |> Array.map(fun xi -> xi |> Array.append([|1.0|]))
    let theta =
        x
        |> Array.head
        |> Array.map(fun _ -> random.NextDouble())
    
    run x y theta 0 0.0

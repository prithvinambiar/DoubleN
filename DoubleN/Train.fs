module DoubleN.Train

let cost actual predicted =
    let squaredError a b =
        (b - a) ** 2.0
    let m = actual |> Seq.length |> float

    actual
    |> Seq.map2 squaredError predicted
    |> Seq.reduce(+)
    |> fun res -> res / (2.0 * m)

let costDerivative input actual predicted =
    let m = actual |> Seq.length |> float

    actual
    |> Seq.map2(-) predicted
    |> Seq.map2(*) input
    |> Seq.reduce(+)
    |> fun res -> res / m

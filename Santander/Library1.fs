module Santander.Solve

open FSharp.Data
open DoubleN.LogisticRegression

let toKeep = ["num_var39_0";
              "ind_var13";
              "num_op_var41_comer_ult3";
              "num_var43_recib_ult1";
              "imp_op_var41_comer_ult3";
              "num_var8";
              "num_var42";
              "num_var30";
              "saldo_var8";
              "num_op_var39_efect_ult3";
              "num_op_var39_comer_ult3";
              "num_var41_0";
              "num_op_var39_ult3";
              "saldo_var13";
              "num_var30_0";
              "ind_var37_cte";
              "ind_var39_0";
              "num_var5";
              "ind_var10_ult1";
              "num_op_var39_hace2";
              "num_var22_hace2";
              "num_var35";
              "ind_var30";
              "num_med_var22_ult3";
              "imp_op_var41_efect_ult1";
              "var36";
              "num_med_var45_ult3";
              "imp_op_var39_ult1";
              "imp_op_var39_comer_ult3";
              "imp_trans_var37_ult1";
              "num_var5_0";
              "num_var45_ult1";
              "ind_var41_0";
              "imp_op_var41_ult1";
              "num_var8_0";
              "imp_op_var41_efect_ult3";
              "num_op_var41_ult3";
              "num_var22_hace3";
              "num_var4";
              "imp_op_var39_comer_ult1";
              "num_var45_ult3";
              "ind_var5";
              "imp_op_var39_efect_ult3";
              "num_meses_var5_ult3";
              "saldo_var42";
              "imp_op_var39_efect_ult1";
              "PCATwo";
              "num_var45_hace2";
              "num_var22_ult1";
              "saldo_medio_var5_ult1";
              "PCAOne";
              "saldo_var5";
              "ind_var8_0";
              "ind_var5_0";
              "num_meses_var39_vig_ult3";
              "saldo_medio_var5_ult3";
              "num_var45_hace3";
              "num_var22_ult3";
              "saldo_medio_var5_hace3";
              "saldo_medio_var5_hace2";
              "SumZeros";
              "saldo_var30";
              "var38";
              "var15"]

let scale avg min max value =
    match max - min with
    | 0.0 -> value
    | _ as divisor -> (value - avg) / divisor

let scaleFeature (values : float [][]) =
    let length = values.[0].Length
    let avgs = [|0..length-1|] |> Array.map(fun a -> values |> Array.map(fun x -> x.[a]) |> Array.average)
    let mins = [|0..length-1|] |> Array.map(fun a -> values |> Array.map(fun x -> x.[a]) |> Array.min)
    let maxs = [|0..length-1|] |> Array.map(fun a -> values |> Array.map(fun x -> x.[a]) |> Array.max)
    
    values |> Array.map(fun x -> x |> Array.mapi(fun i xi-> scale avgs.[i] mins.[i] maxs.[i] xi))

let parseColumns (headers:string[]) (row:CsvRow) =
    headers
    |> Seq.filter (fun header -> toKeep |> List.contains header)
    |> Seq.map(fun header -> row.GetColumn header |> System.Double.Parse)
    |> Array.ofSeq
    ,
    row.GetColumn "TARGET" |> System.Double.Parse

let file = CsvFile.Load @"D:\New_folder\personal\prithvi\2016\Kaggle\Santander\data\train.csv"
let headers = match file.Headers with | Some str -> str | None -> [||]

let inputs = 
    file.Rows
    |> Seq.map(parseColumns headers)
    

let x =
    inputs
    |> Seq.map(fun (a,b) -> a)
    |> Array.ofSeq
    |> scaleFeature

let y =
    inputs
    |> Seq.map(fun (a,b) -> b)
    |> Array.ofSeq

let theta = regress x y


//========================================================== output

let parseColumns' (headers:string[]) (row:CsvRow) =
    headers
    |> Seq.filter (fun header -> toKeep |> List.contains header)
    |> Seq.map(fun header -> row.GetColumn header |> System.Double.Parse)
    |> Array.ofSeq
    |> Array.append ([|1.0|])
    ,
    row.GetColumn "ID" |> System.Int32.Parse

let file' = CsvFile.Load @"D:\New_folder\personal\prithvi\2016\Kaggle\Santander\data\test.csv"
let headers' = match file.Headers with | Some str -> str | None -> [||]

let inputs' = 
    file'.Rows
    |> Seq.map(parseColumns' headers)

let length = x.[0].Length
let avgs = [|0..length-1|] |> Array.map(fun a -> x |> Array.map(fun xi -> xi.[a]) |> Array.average)
let mins = [|0..length-1|] |> Array.map(fun a -> x |> Array.map(fun xi -> xi.[a]) |> Array.min)
let maxs = [|0..length-1|] |> Array.map(fun a -> x |> Array.map(fun xi -> xi.[a]) |> Array.max)


let t =
    inputs'
    |> Seq.map(fun (a,b) ->
        a 
        |> Array.mapi(fun i xi -> scale avgs.[i] mins.[i] maxs.[i] xi)
        |> fun a -> b, sigmoid a theta
      )

let f = new System.IO.StreamWriter @"D:\New_folder\personal\prithvi\2016\Kaggle\Santander\data\submit2.csv"
t |> Seq.iter(fun (a, b) -> f.WriteLine (sprintf "%d,%f" a b))
f.Flush()
f.Dispose()

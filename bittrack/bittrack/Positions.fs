module Positions
    open Assets

    //Amount DU
    type Amount  = | Amount of Asset * decimal
    with 
        static member (+) (am1:Amount, am2:Amount) = 
            match (am1, am2) with
            | (Amount (a1, v1), Amount(a2, v2)) when a1 = a2 -> Some(Amount (a1, v1 + v2))
            | _ -> None
        static member (-) (am1:Amount, am2:Amount) = 
            match (am1, am2) with
            | (Amount (a1, v1), Amount(a2, v2)) when a1 = a2 -> Some(Amount (a1, v1 - v2))
            | _ -> None

    let mapValue fn = function | Amount (a, v) -> Amount(a,  fn v)

    //PositionDU
    type Position = |Position of Map<Asset, decimal>

    let toAmountList (p:Position) =
        p |> function | Position m -> (m |> Map.toSeq |> Seq.map Amount) |> Seq.toList


    let toPosition (amounts: Amount list) = 
        amounts |> Seq.map (function | Amount(a, v) -> (a, v))
        |> Seq.groupBy fst
        |> Seq.map (fun (a, values) -> (a, values |> Seq.sumBy snd))
        |> Map.ofSeq |> Position


    member Position
    with
        static member (+) (p1:Position, p2:Position) = 
                List.append (p1 |> toAmountList) (p2 |> toAmountList) |> toPosition

    member Position with 



    let (*) am (m:decimal)= mapValue (fun x-> x * m) am
    let (/) am (m:decimal)= mapValue (fun x-> x / m) am







(-)
let add a b = a + b

add
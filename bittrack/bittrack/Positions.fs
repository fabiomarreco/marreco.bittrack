namespace Marreco.BitTrack
open Assets
module Amount =
    //Amount DU
    type Amount  = | Amount of Asset * decimal
    let mapValue fn = function | Amount (a, v) -> Amount(a,  fn v)

    type Amount with//redefining artithmetics operators
        static member (+) (am1:Amount, am2:Amount) = 
            match (am1, am2) with
            | (Amount (a1, v1), Amount(a2, v2)) when a1 = a2 -> Some(Amount (a1, v1 + v2))
            | _ -> None
        static member (-) (am1:Amount, am2:Amount) = 
            match (am1, am2) with
            | (Amount (a1, v1), Amount(a2, v2)) when a1 = a2 -> Some(Amount (a1, v1 - v2))
            | _ -> None
        static member (*) (am, m:decimal)= mapValue (fun x-> x * m) am
        static member (/) (am, m:decimal)= mapValue (fun x-> x / m) am

module Position =
    open Amount
    //PositionDU
    type Position = |Position of Map<Asset, decimal>

    let toAmountSeq (p:Position) =
        p |> function | Position m -> (m |> Map.toSeq |> Seq.map Amount)

    let ofAmountSeq (amounts: Amount seq) = 
        amounts |> Seq.map (function | Amount(a, v) -> (a, v))
        |> Seq.groupBy fst
        |> Seq.map (fun (a, values) -> (a, values |> Seq.sumBy snd))
        |> Map.ofSeq |> Position

    let merge positions = 
        Seq.collect (toAmountSeq) positions |> ofAmountSeq

    let map p fn = toAmountSeq p |> Seq.map fn |> ofAmountSeq

    type Position with // redefining operators
        static member (+) (p1:Position, p2:Position) = 
                [p1; p2] |> merge
        static member (*) (p1:Position, m:decimal) = map p1 (mapValue (fun x -> m * x))
        static member (-) (p1:Position, p2:Position) = 
            let invp2 =   (*) -1M |> mapValue |> map p2
            [p1; invp2] |> merge


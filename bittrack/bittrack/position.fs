namespace Marreco.BitTrack
module Position =
    open Assets
    open Amount
    //PositionDU
    type Position = |Position of Map<Asset, decimal>

    let empty = Position (Map.empty)

    let toAmountSeq (p:Position) =
        p |> function | Position m -> (m |> Map.toSeq |> Seq.map Amount)

    let ofAmountSeq (amounts: Amount seq) = 
        amounts |> Seq.map (function | Amount(a, v) -> (a, v))
        |> Seq.groupBy fst
        |> Seq.map (fun (a, values) -> (a, values |> Seq.sumBy snd))
        |> Seq.filter (fun (_, v) -> v <> 0M)
        |> Map.ofSeq |> Position

    let merge positions = 
        Seq.collect (toAmountSeq) positions |> ofAmountSeq

    let map p fn = toAmountSeq p |> Seq.map fn |> ofAmountSeq

    let position seq = seq |> Seq.map (fun (a, v) -> Amount (a, v)) |> ofAmountSeq

    type Position with // redefining operators
        static member (+) (p:Position, am:Amount) = 
            p |> toAmountSeq |> Seq.append [am ] |> ofAmountSeq        
        static member (+) (p1:Position, p2:Position) = 
                [p1; p2] |> merge
        static member (*) (p1:Position, m:decimal) = map p1 (mapValue (fun x -> m * x))
        static member (-) (p1:Position, p2:Position) = 
            let invp2 =   (*) -1M |> mapValue |> map p2
            [p1; invp2] |> merge
        static member (-) (p:Position, am:Amount) = 
            p + (am * -1M)        

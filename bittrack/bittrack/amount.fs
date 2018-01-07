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


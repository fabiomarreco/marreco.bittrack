namespace Marreco.BitTrack
open System
open System.Transactions

module Transaction = 
    open Amount

    type Address = | Address of System.Guid
    type OutputId  = | OutputId of Guid
    type TransactionId = | TransactionId of Guid

    type Output = { Id : OutputId; Qtd : Amount; Address : Address}
    type Transaction = { Id : TransactionId; When : DateTimeOffset; In : Output list; Out : Output list }

    let (|OutputPosition|_|) (outputs:Output seq) = 
        outputs |> Seq.map (fun x -> x.Qtd) |> Position.ofAmountSeq |> Some

    let validTrasaction  = function 
     | { Id = TransactionId guid } when guid = Guid.Empty -> None // invalid transactionId
     | { In = OutputPosition posIn; Out = OutputPosition posOut} when posIn <> posOut -> None
     | Transaction t -> Some(t)



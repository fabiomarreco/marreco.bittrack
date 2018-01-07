namespace Marreco.BitTrack
open System

module Transaction = 
    open Amount

    type Address = | Address of System.Guid
    type OutputId  = | OutputId of Guid
    type TransactionId = | TransactionId of Guid

    type Output = { Id : OutputId; Amount : Amount; Address : Address}
    type Transaction = { Id : TransactionId; Inputs : Transaction list; Outputs : Output list }

    let validTrasaction  = function 
     | { Id = id; Inputs = inputs; Outputs  = outputs} -> Some ({Id = id; Inputs = inputs; Outputs =  outputs})
     | _ -> None



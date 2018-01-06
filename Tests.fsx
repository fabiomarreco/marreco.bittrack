module Transaction

//Domain module testing
open System;

type Asset = 
    | USD
    | BTC
    | BTG
    | ETH
    | SNT
    | XMR
    | NEO


type Address = | Address of System.Guid
let newAddress() = Guid.NewGuid() |> Address
type Amount  = | Amount of Asset * decimal
type OutputId  = | OutputId of Guid
type TransactionId = | TransactionId of Guid

type Output = { Id : OutputId; Amount : Amount; Address : Address}
let newOutputId() = Guid.NewGuid() |> OutputId;
type Transaction = { Id : TransactionId; Inputs : Transaction list; Outputs : Output list }
let newTransactionId() = Guid.NewGuid() |> TransactionId

let validTrasaction  = function 
 | { Id = id; Inputs = inputs; Outputs  = outputs} -> Some ({Id = id; Inputs = inputs; Outputs =  outputs})
 | _ -> None


//------- testings
let addr1 = newAddress()
let addr2 = newAddress()
let addr3 = newAddress()

let coinBase = {
        Id = newTransactionId(); 
        Inputs = []; 
        Outputs = [ {Id = newOutputId(); Amount = Amount (BTC, 12.5M); Address = addr1} ] 
    }


let transaction2 = {
    Id = newTransactionId();
    Inputs = [ coinBase ];
    Outputs = [ 
        { Id = newOutputId(); Amount = Amount (BTC, 10M); Address = addr2};
        { Id = newOutputId(); Amount = Amount (BTC, 2.5M); Address = addr3} ]
    }        

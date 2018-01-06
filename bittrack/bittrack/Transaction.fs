module Transaction

open Currencies;
open Accounts;
open System;

type Asset = 
    | USD
    | BTC
    | BTG
    | ETH
    | SNT
    | XMR
    | NEO

type Amount  = | Amount of Asset * decimal
type OutputId  = | OutputId of Guid
type TransactionId = | TransactionId of Guid

type Output = { Id : OutputId; Amount : Amount; Address : Address}
type Transaction = { Id : TransactionId; Inputs : Transaction list; Outputs : Output list }


let validTrasaction  = function 
 | { Id = id; Inputs = inputs; Outputs  = outputs} -> Some ({Id = id; Inputs = inputs; Outputs =  outputs})
 | _ -> None



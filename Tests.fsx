module Transaction


//Domain module testing
open System;

module Teste = 
    type T1 = private |T1 of int
    let create i = T1 (i)


open Teste

let t = create (2)
let x = match t with 
        | T1 a -> a





type Address = | Address of System.Guid
let newAddress() = Guid.NewGuid() |> Address

type Amount  = | Amount of Asset * decimal
type OutputId  = | OutputId of Guid
type TransactionId = | TransactionId of Guid

type Output = { Id : OutputId; Amount : Amount; Address : Address}
let newOutputId() = Guid.NewGuid() |> OutputId;
type Transaction = { Id : TransactionId; Inputs : Output list; Outputs : Output list }
let newTransactionId() = Guid.NewGuid() |> TransactionId

type Position = |Position of Map<Asset, decimal>

let toPosition (amounts: Amount list) = 
    amounts |> Seq.map (function | Amount(a, v) -> (a, v))
    |> Seq.groupBy fst
    |> Seq.map (fun (a, values) -> (a, values |> Seq.sumBy snd))
    |> Map.ofSeq |> Position

let toAmountList (p:Position) =
    p |> function | Position m -> (m |> Map.toSeq |> Seq.map Amount) |> Seq.toList

let (+) (p1:Position) (p2:Position) = 
    List.append (p1 |> toAmountList) (p2 |> toAmountList) |> toPosition


let (+) (am1:Amount) (am2:Amount) = 
    match (am1, am2) with
    | (Amount (a1, v1), Amount(a2, v2)) when a1 = a2 -> Some(Amount (a1, v1 + v2))
    | _ -> None

let a = Amount (BTC, 12M)

a |> function | Amount (x, v) -> x

let (|ConsolidatedOutputs|_|) (outs :Output list) = 
    outs |> Seq.map (fun x-> x.Amount) 
    |> Seq.groupBy (function | Amount (a, _)  -> a)
    |> Seq.map (fun (a, v) -> (a, v|> Seq.reduce  (+)))


List.choose
    
    |> Seq.groupBy (function | Amount (asset, _) -> asset) 
         |> Seq.map (fun (k,x) -> k, (x |> function Amount | ))
    
    
     |> Seq.map (function | Amount (a, v) -> v) |> 

//    outs |> Seq.map (fun x-> x.Amount) |> Seq.groupBy (function | Amount (x, _) -> x) |> List.coll


let validTrasaction  = function 
 | { Id = TransactionId id; Inputs = _; Outputs  = _} when id = Guid.Empty -> None // must have Id
 | Consolida
 
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



let outs = [ {Id = newOutputId(); Amount = Amount (BTC, 12.5M); Address = addr1} ] 


let m1 = Map.empty.Add("hwllo", "world").Add("hwllo2", "world3")
let m2 = Map.empty.Add("hwllo5", "world3asdsda")

let amounts = [ Amount (BTC, 12.5M); Amount(BTC, 10M); Amount(ETH, 4M)]

let p = amounts |> toPosition

m1 |> Map.toSeq


[1; 2; 3; 4; 5] |> Seq.fold 0 (fun a b -> a +)
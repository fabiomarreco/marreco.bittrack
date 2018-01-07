namespace Marreco.BitTrack

(*

#r "bin\\debug\\Expecto.dll"
*)

open Expecto
open Amount
open Assets 

module PositionsTests = 
  [<Tests>]
  let positionTests = 
    testList "amount" [
        testCase "maping value to function changes amount" <| fun _ -> 
          let result = Amount (BTC,12.5M) |>  Amount.mapValue (fun x-> x * 2M) 
          let expected = Amount (BTC, 25M)
          Expect.equal result expected "value should be multiplied"
        
        testCase "multiplying by value changes the result amount" <| fun _ -> 
          let result = Amount (BTC,12.5M)  * 2M
          let expected = Amount (BTC, 25M)
          Expect.equal result expected "value should be multiplied"
        
        testCase "dividing by value changes the result amount" <| fun _ -> 
          let result = Amount (BTC,13M)  / 2M
          let expected = Amount (BTC, 6.5M)
          Expect.equal result expected "value should be multiplied"
        
        testCase "Adding 2 amounts same asset adds value" <| fun _ -> 
          let result = Amount (BTC,12M)  + Amount (BTC, 3M)
          let expected = Amount (BTC, 15M) |> Some
          Expect.equal result expected "value should be multiplied"

        testCase "Adding 2 amounts different returns none" <| fun _ -> 
          let result = Amount (BTC,12M) + Amount (ETH, 3M)
          Expect.equal result None "value should be multiplied"
        
        testCase "Subtracting 2 amounts different returns none" <| fun _ -> 
          let result = Amount (BTC,12M) - Amount (ETH, 3M)
          Expect.equal result None "value should be multiplied"
    ]

    testList "position" [
         testCase "Empty amount sequence turns empty position" <| fun _ -> 
           let result = [] |> Position.ofAmountSeq
           Expect.equal result Position.empty "position should be empty"

    ]

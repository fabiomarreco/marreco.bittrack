namespace Marreco.BitTrack

(*

#r "bin\\debug\\Expecto.dll"
*)

open Expecto
open Amount
open Assets
open Position

module PositionsTests = 
  [<Tests>]
  let tests = 
    testList "position" [
         testCase "Empty amount sequence turns empty position" <| fun _ -> 
           let result = [] |> Position.ofAmountSeq
           Expect.equal result Position.empty "position should be empty"

         testCase "single amount sequence turns single position" <| fun _ -> 
           let result = [Amount (BTC, 12M)] |> Position.ofAmountSeq
           Expect.notEqual result Position.empty "position should not be empty"
           let expected  = position [ (BTC, 12M)]
           Expect.equal result expected "Position should contain 1 value"

         testCase "amounts of same asset should be added" <| fun _ -> 
           let pos = position [ (BTC, 12M); (BTC, 3M); (ETH, 3M)]
           let expected = position [ (BTC, 15M); (ETH, 3M)]
           Expect.equal pos expected "Amounts should have been added"

         testCase "Amount sequence from position generates unique" <| fun _ -> 
           let actual = position [ (BTC, 12M); (ETH, 3M)] |> toAmountSeq 
           let expected = [ Amount (BTC, 12M); Amount (ETH, 3M)]
           Expect.sequenceEqual expected actual "Sequences should be equal"

         testCase "merge of positions generates new one" <| fun _ -> 
           let p1 = position [ (BTC, 12M); (ETH, 3M)]
           let p2 = position [ (BTC, 10M); (ETH, 2M); (XMR, 3M)]
           let result = [p1; p2] |> merge
           let expected = position [ (BTC, 22M); (ETH, 5M); (XMR, 3M)]
           Expect.equal expected result "Amounts should have been added"
           
         testCase "adding of positions generates new one" <| fun _ -> 
           let p1 = position [ (BTC, 12M); (ETH, 3M)]
           let p2 = position [ (BTC, 10M); (ETH, 2M); (XMR, 3M)]
           let result = p1 + p2
           let expected = position [ (BTC, 22M); (ETH, 5M); (XMR, 3M)]
           Expect.equal expected result "Amounts should have been added"
           
         testCase "subtract of positions generates new one" <| fun _ -> 
           let p1 = position [ (BTC, 12M); (ETH, 3M)]
           let p2 = position [ (BTC, 10M); (ETH, 2M); (XMR, 3M)]
           let result = p1 - p2
           let expected = position [ (BTC, 2M); (ETH, 1M); (XMR, -3M)]
           Expect.equal expected result "Amounts should have been subtracted"

         testCase "subtract of equal positions creates empty one" <| fun _ -> 
           let p = position [ (BTC, 12M); (ETH, 3M)]
           let result = p - p
           Expect.equal Position.empty result "Position should be empty"

         testCase "Multiplication by one gives same position" <| fun _ -> 
           let p = position [ (BTC, 12M); (ETH, 3M)]
           let result = p * 1M
           Expect.equal p result "Position be equal"
         
         testCase "Multiplication by two gives new position" <| fun _ -> 
           let p = position [ (BTC, 12M); (ETH, 3M)]
           let result = p * 2M
           let expected = position [(BTC, 24M); (ETH, 6M)]
           Expect.equal expected result "Position be equal to expected"

         testCase "Adding new amount diffent asset to position creates new one" <| fun _ -> 
           let pos = position [ (BTC, 12M)]
           let am = Amount (ETH, 3M)
           let result = pos + am
           let expected = position [ (BTC, 12M); (ETH, 3M)]
           Expect.equal expected result "Should be equal to expected"
         
         testCase "Subtracting new amount diffent asset to position creates new one" <| fun _ -> 
           let pos = position [ (BTC, 12M)]
           let am = Amount (ETH, 3M)
           let result = pos - am
           let expected = position [ (BTC, 12M); (ETH, -3M)]
           Expect.equal expected result "Should be equal to expected"
           
         testCase "Subtracting current amount yield zero position" <| fun _ -> 
           let pos = position [ (BTC, 12M)]
           let am = Amount (BTC, 12M)
           let result = pos - am
           Expect.equal Position.empty result "Should be equal to expected"
    ]

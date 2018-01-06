module Accounts
open System
    type Account(id:string) =
        member this.Id = id
    
    type Address = | Address of System.Guid
// Learn more about F# at http://fsharp.org

open System
open FParsec
open Attributes
open Elements

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let attribute = "height=\"736\""

[<EntryPoint>]
let main argv =
    test pheight attribute
    0 // return an integer exit code

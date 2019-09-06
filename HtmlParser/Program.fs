// Learn more about F# at http://fsharp.org

open System
open FParsec
open Attributes

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

[<EntryPoint>]
let main argv =
    test pclass "id=\"Download the Case Study\"" 
    0 // return an integer exit code

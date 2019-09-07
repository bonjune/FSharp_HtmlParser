// Learn more about F# at http://fsharp.org

open System
open FParsec
open Attributes
open Tags

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let tag = "<ul><li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/agile/\" role=\"menuitem\">Agile</a></li>" +
            "<li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/cloud\" role=\"menuitem\">Cloud</a></li></ul>"

[<EntryPoint>]
let main argv =
    test pul tag
    0 // return an integer exit code

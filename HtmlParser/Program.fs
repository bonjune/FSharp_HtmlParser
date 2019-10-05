// Learn more about F# at http://fsharp.org

open System
open FParsec
open Attributes
open Elements
open System.Text

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let testFile p path =
    match runParserOnFile p () path (UTF8Encoding()) with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let element = "<span style=\"color: #ffffff;\">Fortune 500 Company Advances in Retail Technology with Remote Software Team</span>"

[<EntryPoint>]
let main argv =
    test pspan element
    //let filePath = "C:\Users\Jake\Desktop\callibrity.html"
    //testFile pelements filePath
    0 // return an integer exit code

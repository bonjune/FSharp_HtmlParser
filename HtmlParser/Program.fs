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

let attribute = "sizes=\"(max-width: 162px) 100vw, 162px\""

[<EntryPoint>]
let main argv =
    //let filePath = "C:\Users\Jake\Desktop\callibrity.html"
    //testFile pelements filePath
    test psizes attribute
    0 // return an integer exit code

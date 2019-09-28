module TestUtilities

open FParsec
open HtmlParser.Types

let attributeTest p str =
    match run p str with 
    | Success(result, _, _)   -> result 
    | _ -> AttributeError

let elementTest p str =
    match run p str with 
    | Success(result, _, _)   -> result 
    | _ -> ElementError


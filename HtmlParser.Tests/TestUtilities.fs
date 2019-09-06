module TestUtilities

open FParsec

let test p str =
    match run p str with 
    | Success(result, _, _)   -> result 
    | Failure(errorMsg, _, _) -> errorMsg


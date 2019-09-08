module TestUtilities

open FParsec
open HtmlParser.Types

let test p str err =
    match run p str with 
    | Success(result, _, _)   -> result 
    | _ -> err

let tag : Tag = { Attributes = []; Content = [] }
let tagErr constr = constr tag

let sctag : ScTag = { Attributes = [] }
let scTagErr constr = constr sctag


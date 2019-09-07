module Attributes

open FParsec
open HtmlParser.Types

let pvalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c)

let pvalueidentifier : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let pattribute name value constr : Parser<_> = 
    pstring (name + "=\"") >>. value .>> pstring "\"" .>> spaces |>> constr

// attrubutes
let pclass : Parser<_> = pattribute "class" (many1 pvalueidentifier) Class

let pid : Parser<_> = pattribute "id" pvalueidentifier Id

let ptitle : Parser<_> = pattribute "title" pvalue Title

let pglobals : Parser<_> =
    choice [pclass; pid; ptitle]
    
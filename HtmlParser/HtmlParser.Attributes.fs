module Attributes

open FParsec
open HtmlParser.Types

// class attributes
let pclassattribute : Parser<_> =
    pstring "class=\""

let pclasscontent : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let pclass : Parser<_> = 
    pclassattribute >>. many1 pclasscontent .>> pstring "\"" .>> spaces |>> Class

// id attribute
let pidattribute : Parser<_> =
    pstring "id=\""

let pidcontent : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let pid : Parser<_> =
    pidattribute >>. pidcontent .>> pstring "\"" .>> spaces |>> Id

// title attribute
let ptitleattribute : Parser<_> =
    pstring "title=\""

let ptitlecontent : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_ \n" c || isLetter c || isDigit c)

let ptitle : Parser<_> =
    ptitleattribute >>. ptitlecontent .>> pstring "\"" .>> spaces |>> Title

let pglobals : Parser<_> =
    choice [pclass; pid; ptitle]
    
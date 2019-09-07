module Attributes

open FParsec
open HtmlParser.Types

let pvalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c)

let pidentifiervalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let purlvalue : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_,./?;:!@#$%^&*()+=-" c || isLetter c || isDigit c)

let purl : Parser<_> =
    pipe2 (pstring "http://" <|> pstring "https://") purlvalue (fun h v -> h + v)

let pattrname name : Parser<_> = pstring (name + "=\"")

let pdataname : Parser<_> = pipe2 (pstring "data-") (pidentifiervalue .>> pstring "=\"") (fun d i -> d + i)

let pattribute name value constr : Parser<_> = 
    name >>. value .>> pstring "\"" .>> spaces |>> constr

// attrubutes
let pclass : Parser<_> = pattribute (pattrname "class") (many1 pidentifiervalue) Class

let pid : Parser<_> = pattribute (pattrname "id") pidentifiervalue Id

let ptitle : Parser<_> = pattribute (pattrname "title") pvalue Title

let pdata : Parser<_> = tuple2 pdataname pvalue .>> pstring "\"" .>> spaces |>> Data

let phref : Parser<_> = pattribute (pattrname "href") purl Href
    
module Attributes

open FParsec
open HtmlParser.Types

let mapPstringChoice : (string list -> Parser<_>) = choice << (List.map pstring)

let pcontentValue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c)

let pidentifiervalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let prolevalue : Parser<_> =
    mapPstringChoice ["banner"; "button"; "complementary"; "contentinfo"; "definition"; "main"; "menuitem"; "menu"; "navigation"; "note"; "progressbar"; "search"] .>> spaces

let ptypeValue : Parser<_> =
    mapPstringChoice ["button"; "checkbox"; "color"; "datetime-local"; "date"; "email"; "file"; "hidden"; "image"; "month"; "number"; "password"; "radio"; "range"; "reset"; "search"; "submit"; "tel"; "text/javascript"; "text"; "time"; "url"; "video/mp4"; "week"]

let purlvalue : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_,./?;:!@#$%^&*()+=-" c || isLetter c || isDigit c)

let purl : Parser<_> =
    pipe2 (mapPstringChoice ["http://"; "https://"; "#"; "/";]) purlvalue (fun h v -> h + v)

let pattrname name : Parser<_> = pstring (name + "=\"")

let pdataname : Parser<_> = pipe2 (pstring "data-") (pidentifiervalue .>> pstring "=\"") (fun d i -> d + i)

let pariatype : Parser<_> = mapPstringChoice ["label"; "haspopup"; "expanded"; "valuemin"; "valuemax"; "valuenow"; ]

let parianame : Parser<_> = pipe2 (pstring "aria-") (pariatype .>> pstring "=\"") (fun a i -> a + i)

let pattribute name value constr : Parser<_> = 
    name >>. value .>> pstring "\"" .>> spaces |>> constr

// attrubutes
let pclass : Parser<_> = pattribute (pattrname "class") (many1 pidentifiervalue) Class

let pid : Parser<_> = pattribute (pattrname "id") pidentifiervalue Id

let ptitle : Parser<_> = pattribute (pattrname "title") pcontentValue Title

let pdata : Parser<_> = tuple2 pdataname pcontentValue .>> pstring "\"" .>> spaces |>> Data

let phref : Parser<_> = pattribute (pattrname "href") purl Href

let prole : Parser<_> = pattribute (pattrname "role") (many1 prolevalue) Role
   
let paria : Parser<_> = tuple2 parianame pcontentValue .>> pstring "\"" .>> spaces |>> Aria 

let pvalue : Parser<_> = pattribute (pattrname "value") pcontentValue Value

let ptype : Parser<_> = pattribute (pattrname "type") ptypeValue Type
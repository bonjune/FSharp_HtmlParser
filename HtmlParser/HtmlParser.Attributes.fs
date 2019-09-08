module Attributes

open FParsec
open HtmlParser.Types

let mapPstringChoice : (string list -> Parser<_>) = choice << (List.map pstring)

let pcontentvalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c)

let pnumbervalue : Parser<_> =
    many1Satisfy isDigit

let pidentifiervalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let ppreloadvalue : Parser<_> =
    (mapPstringChoice ["auto"; "none"; "metadata"]) <|> (stringReturn "" "auto")

let roleOptions = ["banner"; "button"; "complementary"; "contentinfo"; "definition"; "main"; "menuitem"; "menu"; "navigation"; "note"; "progressbar"; "search"]

let prolevalue : Parser<_> =
    mapPstringChoice roleOptions .>> spaces

let typeOptions = ["button"; "checkbox"; "color"; "datetime-local"; "date"; "email"; "file"; "hidden"; "image"; "month"; "number"; "password"; "radio"; "range"; "reset"; "search"; "submit"; "tel"; "text/javascript"; "text"; "time"; "url"; "video/mp4"; "week"]

let ptypevalue : Parser<_> =
    mapPstringChoice typeOptions

let pbooleanvalue : Parser<_> = pstring "true" <|> pstring "false" <|> pstring ""

let purlvalue : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_,./?;:!@#$%^&*()+=-" c || isLetter c || isDigit c)

let purl : Parser<_> =
    pipe2 (mapPstringChoice ["http://"; "https://"; "#"; "/";]) purlvalue (fun h v -> h + v)

let pattrname name : Parser<_> = pstring (name + "=\"")

let pdataname : Parser<_> = 
    pipe2 (pstring "data-") (pidentifiervalue .>> pstring "=\"") (fun d i -> d + i)

let pariavalue : Parser<_> = 
    mapPstringChoice ["label"; "haspopup"; "expanded"; "valuemin"; "valuemax"; "valuenow"; ]

let parianame : Parser<_> = 
    pipe2 (pstring "aria-") (pariavalue .>> pstring "=\"") (fun a i -> a + i)

let pattribute name value constr : Parser<_> = 
    name >>. value .>> pstring "\"" .>> spaces |>> constr

// attrubutes
let pclass = pattribute (pattrname "class") (many1 pidentifiervalue) Class

let pid = pattribute (pattrname "id") pidentifiervalue Id

let ptitle = pattribute (pattrname "title") pcontentvalue Title

let pdata = tuple2 pdataname pcontentvalue .>> pstring "\"" .>> spaces |>> Data

let phref = pattribute (pattrname "href") purl Href

let prole = pattribute (pattrname "role") (many1 prolevalue) Role
   
let paria = tuple2 parianame pcontentvalue .>> pstring "\"" .>> spaces |>> Aria 

let pvalue = pattribute (pattrname "value") pcontentvalue Value

let ptype = pattribute (pattrname "type") ptypevalue Type

let pautoplay = pattribute (pattrname "autoplay") pbooleanvalue Autoplay

let ploop = pattribute (pattrname "loop") pbooleanvalue Loop

let pmuted = pattribute (pattrname "muted") pbooleanvalue Muted

let ppreload = pattribute (pattrname "preload") ppreloadvalue Preload

let psrc = pattribute (pattrname "src") (purlvalue) Src

let palt = pattribute (pattrname "alt") pcontentvalue Alt

let pheight = pattribute (pattrname "height") pnumbervalue Height

let pwidth = pattribute (pattrname "width") pnumbervalue Width
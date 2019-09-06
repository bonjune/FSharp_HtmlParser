module Tags

open FParsec
open HtmlParser.Types
open Attributes

let ptags, ptagsRef = createParserForwardedToRef<HtmlTags list, unit>()

// body tag
let pbodyopentag : Parser<_> = pstring "<body"
let pbodyclosetag : Parser<_> = pstring "</body>"
let pbodyattributes : Parser<_> = many pglobals
let pbody : Parser<HtmlTags> = 
    pipe3 (pbodyopentag >>. spaces >>. pbodyattributes .>> pstring ">")
          ptags  
          pbodyclosetag 
          (fun a t _ -> HtmlBody { Attributes = a; Tags = t })

// div tag
let pdivopentag : Parser<_> = pstring "<div"
let pdivclosetag : Parser<_> = pstring "</div>"
let pdivattributes : Parser<_> = many pglobals
let pdiv : Parser<_> =
    pipe3 (pdivopentag >>. spaces >>. pbodyattributes .>> pstring ">")
           ptags
           pdivclosetag 
           (fun a t _ -> HtmlDiv{ Attributes = a; Tags = t })

do ptagsRef := many (choice [pbody; pdiv;])
module Tags

open FParsec
open HtmlParser.Types
open Attributes

// body tag
let pbodyopentag : Parser<_> = pstring "<body"
let pbodyclosetag : Parser<_> = pstring "</body>"
let pbodyattributes : Parser<_> = many pglobals
let pbody : Parser<_> = 
    pipe2 (pbodyopentag >>. spaces >>. pbodyattributes .>> pstring ">") 
          pbodyclosetag 
          (fun a t -> { Attributes = a; Tags = [] })
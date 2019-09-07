module Tags

open FParsec
open HtmlParser.Types
open Attributes

let ptags, ptagsRef = createParserForwardedToRef<TagContent list, unit>()

let pcontent : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c) |>> (fun r -> [Content r] )

let tagConstructor constr a t _ = constr { Attributes = a; Content = t }

let ptag name attr constr : Parser<_> =
    pipe3 (pstring ("<" + name) >>. spaces >>. (many attr) .>> pstring ">")
          (ptags <|> pcontent)
          (pstring ("</" + name + ">"))
          (tagConstructor constr)

// attribute choices
let pglobalattr : Parser<_> =
    choice [pclass; pid; ptitle]

// tags
let pbody : Parser<TagContent> = ptag "body" pglobalattr Body

let pdiv : Parser<_> = ptag "div" pglobalattr Div

do ptagsRef := many (choice [pbody; pdiv;])
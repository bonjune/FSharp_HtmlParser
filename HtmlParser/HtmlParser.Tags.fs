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
          (pcontent <|> ptags)
          (pstring ("</" + name + ">"))
          (tagConstructor constr)

// attribute choices
let pglobalattr : Parser<_> =
    choice [pclass; pid; ptitle]

let pdivattr : Parser<_> =
    choice [pclass; pid; ptitle; paria]

let patagattr : Parser<_> =
    choice [pclass; pid; ptitle; phref; prole]

let pliattr : Parser<_> =
    choice [pclass; pid; ptitle; pvalue; prole]

// tags
let pbody : Parser<_> = ptag "body" pglobalattr Body

let pdiv : Parser<_> = ptag "div" pdivattr Div

let patag : Parser<_> = ptag "a" patagattr ATag

let pul : Parser<_> = ptag "ul" pglobalattr Ul

let pli : Parser<_> = ptag "li" pliattr Li

do ptagsRef := many (choice [pbody; pdiv; patag; pul; pli;])
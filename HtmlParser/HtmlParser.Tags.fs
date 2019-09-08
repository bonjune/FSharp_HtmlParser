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

let scTagConstructor constr a = constr { Attributes = a }

let psctag name attr constr : Parser<_> =
    pstring ("<" + name) >>. spaces >>. (many attr) .>> pstring "/>" |>> scTagConstructor constr


// attribute options

let globalAttributes = [pclass; pid; ptitle]

let pglobalattr =
    choice globalAttributes

let pdivattr =
    [paria] |> List.append globalAttributes |> choice

let patagattr =
    [phref; prole] |> List.append globalAttributes |> choice

let pliattr =
    [pvalue; prole] |> List.append globalAttributes |> choice

let psourceattr =
    [psrc; ptype] |> List.append globalAttributes |> choice

let pvideoattr =
    [pautoplay; ploop; pmuted; ppreload] |> List.append globalAttributes |> choice 

let pimgattr =
    [palt; pheight; pwidth; psrc] |> List.append globalAttributes |> choice


// tag parsers
let pbody = ptag "body" pglobalattr Body

let pdiv = ptag "div" pdivattr Div

let patag = ptag "a" patagattr ATag

let pul = ptag "ul" pglobalattr Ul

let pli = ptag "li" pliattr Li

let psource = psctag "source" psourceattr Source

let pvideo = ptag "video" pvideoattr Video

let pimg = psctag "img" pimgattr Img

let ph1 = ptag "h1" pglobalattr H1

let ph2 = ptag "h2" pglobalattr H2

let ph3 = ptag "h3" pglobalattr H3

let ph4 = ptag "h4" pglobalattr H4

let ph5 = ptag "h5" pglobalattr H5

let ph6 = ptag "h6" pglobalattr H6

let pptag = ptag "p" pglobalattr PTag

let tagParsers = [pbody; pdiv; patag; pul; pli; psource; pvideo; pimg; ph1; ph2; ph3; ph4; ph5; ph6; pptag]

do ptagsRef := tagParsers |> choice |> many
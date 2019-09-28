﻿module Elements

open FParsec
open HtmlParser.Types
open Attributes

// html parser helpers
let pelements, pelementsRef = createParserForwardedToRef<ElementContent list, unit>()

let pcontent : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c) |>> (fun r -> [Content r] )

let elementConstructor constr a t _ = constr { Attributes = a; Content = t }

let pelement name attr constr : Parser<_> =
    pipe3 (pstring ("<" + name) >>. spaces >>. (many attr) .>> pstring ">")
          (pcontent <|> pelements)
          (pstring ("</" + name + ">"))
          (elementConstructor constr)

let scElementConstructor constr a = constr { Attributes = a }

let pscelement name attr constr : Parser<_> =
    pstring ("<" + name) >>. spaces >>. (many attr) .>> pstring "/>" |>> scElementConstructor constr


// attribute options
let globalAttributes = [pclass; pid; ptitle]

let pglobalattr =
    choice globalAttributes

let pdivattr =
    [paria] @ globalAttributes |> choice

let patagattr =
    [phref; prole] @ globalAttributes |> choice

let pliattr =
    [pvalue; prole] @ globalAttributes |> choice

let psourceattr =
    [psrc; ptype] @ globalAttributes |> choice

let pvideoattr =
    [pautoplay; ploop; pmuted; ppreload] @ globalAttributes |> choice 

let pimgattr =
    [palt; pheight; pwidth; psrc] @ globalAttributes |> choice


// tag parsers
let pbody = pelement "body" pglobalattr Body

let pdiv = pelement "div" pdivattr Div

let paelement = pelement "a" patagattr AElement

let pul = pelement "ul" pglobalattr Ul

let pli = pelement "li" pliattr Li

let psource = pscelement "source" psourceattr Source

let pvideo = pelement "video" pvideoattr Video

let pimg = pscelement "img" pimgattr Img

let ph1 = pelement "h1" pglobalattr H1

let ph2 = pelement "h2" pglobalattr H2

let ph3 = pelement "h3" pglobalattr H3

let ph4 = pelement "h4" pglobalattr H4

let ph5 = pelement "h5" pglobalattr H5

let ph6 = pelement "h6" pglobalattr H6

let ppelement = pelement "p" pglobalattr PElement

let elementParsers = [pbody; pdiv; paelement; pul; pli; psource; pvideo; pimg; ph1; ph2; ph3; ph4; ph5; ph6; ppelement]

do pelementsRef := elementParsers |> choice |> many
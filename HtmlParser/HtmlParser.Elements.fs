module Elements

open System
open FParsec
open HtmlParser.Types
open Attributes

let trim (str : string) = str.Trim()

// element constructors
let elementCtor ctor a c _ = ctor { Attributes = a; Content = c }
let selfClosingCtor ctor a = ctor { Attributes = a }
let contentCtor c = [Content c]

// html parser helpers
let pelements, pelementsRef = createParserForwardedToRef<ElementContent list, unit>()

let pcontent : Parser<_> = 
    many1Satisfy <| isValidChar [isTextSymbol; isLetter; isDigit] |>> contentCtor

let pbeginopentag name =
    pstring <| "<" + name

let pendopentag : Parser<_> = pstring ">"

let popentag name attributeParser =
    pbeginopentag name >>. spaces >>. many attributeParser .>> pendopentag

let pendtag name =
    pstring <| "</" + name + ">"

let pbeginselfclosing = pbeginopentag

let pendselfclosing =
    pstring "/>"

let pelementcontent = 
    spaces >>. pcontent <|> pelements .>> spaces

let pelement name attributeParsers element : Parser<_> =
    pipe3 (popentag name attributeParsers) pelementcontent (pendtag name) (elementCtor element)

let pscelement name attributeParsers element : Parser<_> =
    pbeginselfclosing name >>. spaces >>. many attributeParsers .>> pendselfclosing |>> selfClosingCtor element

let popencomment : Parser<_> = 
    pstring "<!--"

let pclosecomment : Parser<_> = 
    pstring "-->"

let pcomment : Parser<_> = 
    many1Satisfy <| isValidChar [isHtmlCommentSymbol; isLetter; isDigit]

let phtmlcomment' = 
    between popencomment pclosecomment pcomment |>> (trim >> HtmlComment)

let phtmlcomment = 
    spaces >>. phtmlcomment' .>> spaces

// attribute options
let globalAttributes = [pclass; pid; ptitle]

let pglobalattributes =
    choice globalAttributes

let pdivattributes =
    [paria; pdata] @ globalAttributes |> choice

let paelementattributes =
    [phref; prole] @ globalAttributes |> choice

let plineattributes =
    [pvalue; prole] @ globalAttributes |> choice

let psourceattributes =
    [psrc; ptype] @ globalAttributes |> choice

let pvideoattributes =
    [pautoplay; ploop; pmuted; ppreload] @ globalAttributes |> choice 

let pimageattributes =
    [palt; pheight; pwidth; psrc] @ globalAttributes |> choice

// element parsers
let paelement = pelement "a" paelementattributes AElement

let pbody = pelement "body" pglobalattributes Body

let pdiv = pelement "div" pdivattributes Div

let ph1 = pelement "h1" pglobalattributes H1

let ph2 = pelement "h2" pglobalattributes H2

let ph3 = pelement "h3" pglobalattributes H3

let ph4 = pelement "h4" pglobalattributes H4

let ph5 = pelement "h5" pglobalattributes H5

let ph6 = pelement "h6" pglobalattributes H6

let pimg = pscelement "img" pimageattributes Img

let pli = pelement "li" plineattributes Li

let ppelement = pelement "p" pglobalattributes PElement

let pul = pelement "ul" pglobalattributes Ul

let psource = pscelement "source" psourceattributes Source

let pvideo = pelement "video" pvideoattributes Video

let elementParsers = 
    [pbody; pdiv; paelement; pul; pli; psource; pvideo; pimg; ph1; ph2; ph3; ph4; ph5; ph6; ppelement; phtmlcomment]

do pelementsRef := elementParsers |> choice |> many
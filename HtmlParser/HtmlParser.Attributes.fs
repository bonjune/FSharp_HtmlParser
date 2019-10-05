module Attributes

open FParsec
open HtmlParser.Types

// attribute constructors
let imageFileCtor n e = ImageFile { Name = n; Extension = e }
let srcSetCtor s w = { Src = s; Width = w }
let ariaCtor n c = Aria { Name = n; Content = c }
let dataCtor n c = Data { Name = n; Content = c }
let mediaConditionCtor c w = { Condition = c; Size = w }
let sizesCtor m ws = { Media = m; Widths = ws }

// attribute parser helpers
let pbeginattribute name = pstring <| name + "=\""
let pendattribute : Parser<_> = pstring "\""

let pattribute name contentParser ctor : Parser<_> = 
    pbeginattribute name >>. contentParser .>> pendattribute .>> spaces |>> ctor

let mapPstringChoice : string list -> Parser<_> = List.map pstring >> choice

let isValidChar predicates c = 
    List.fold (||) false <| List.map (fun f -> f c) predicates

let isTextSymbol = isAnyOf "_ \n,./?;:|!@#$%^&*()+=-"
let isHtmlCommentSymbol = isAnyOf "_ \n,./?;:|!@#$%^&*()+="
let isUrlSymbol = isAnyOf "_,./?;:!@#$%^&*()+=-"

let roles = ["banner"; "button"; "complementary"; "contentinfo"; "definition"; "main"; "menuitem"; "menu"; "navigation"; "note"; "progressbar"; "search"]
let types = ["button"; "checkbox"; "color"; "datetime-local"; "date"; "email"; "file"; "hidden"; "image"; "month"; "number"; "password"; "radio"; "range"; "reset"; "search"; "submit"; "tel"; "text/javascript"; "text"; "time"; "url"; "video/mp4"; "week"]

let ptrue : Parser<_> = stringReturn "true" true
let pfalse : Parser<_> = stringReturn "false" false
let pnone : Parser<bool option> = stringReturn "" None

let pboolean = choice [ptrue |>> Some; pfalse |>> Some; pnone]

let ptext : Parser<_> = 
    many1Satisfy <| isValidChar [isTextSymbol; isLetter; isDigit]

let pname : Parser<_> = 
    many1Satisfy <| isValidChar [isAnyOf "_-"; isLetter; isDigit] .>> spaces

let purl' : Parser<_> =
    many1Satisfy <| isValidChar [isUrlSymbol; isLetter; isDigit]

let parianame' = 
    mapPstringChoice ["label"; "haspopup"; "expanded"; "valuemin"; "valuemax"; "valuenow"; ]

let pfilename : Parser<_> = pname
let pfileextension : Parser<_> = pchar '.' >>. many1Satisfy isLetter

let pfile : Parser<_> = 
    pipe2 pfilename pfileextension imageFileCtor

let parianame = 
    pstring "aria-" >>. parianame' .>> pstring "=\""

let pclass' = pname |>> ClassAttribute.create |> many

let pdataname : Parser<_> = 
    pstring "data-" >>. pname .>> pstring "=\""

let pmediaunit = mapPstringChoice ["em"; "px"; "vw";]
let pmediawidth = pfloat .>> pmediaunit

let pmediacondition' : Parser<_> = 
    let pmaxwidth = stringReturn "max-width:" MaxWidth
    let pminwidth = stringReturn "min-width:" MinWidth
    (pmaxwidth <|> pminwidth) .>> spaces

let pmediacondition = 
    let pcondition = pipe2 pmediacondition' pmediawidth mediaConditionCtor
    between (pchar '(') (pchar ')') pcondition .>> spaces

let ppreload' : Parser<_> =
    (mapPstringChoice ["auto"; "none"; "metadata"]) <|> (stringReturn "" "auto")

let prole' : Parser<_> =
    mapPstringChoice roles .>> spaces

let ptype' : Parser<_> = 
    mapPstringChoice types

let purl : Parser<_> =
    pipe2 (mapPstringChoice ["http://"; "https://"; "#"; "/";]) purl' (+)

let psizes' = sepBy (pmediawidth |>> ImageWidth) (pchar ',' .>> spaces)
let psizes'' = pipe2 pmediacondition psizes' sizesCtor

let psrcurl = purl |>> (Url >> ImageUrl)
let pimagesrc = spaces >>. (psrcurl <|> pfile) .>> spaces
let psrcwidth : Parser<_> = pfloat .>> pchar 'w' |>> ImageWidth

let psrcsetimage : Parser<_> =
    pipe2 pimagesrc psrcwidth srcSetCtor

let pcustom name ctor = 
    pipe2 name (ptext .>> pendattribute .>> spaces) ctor

let pvaluefloat : Parser<_> = pfloat |>> ValueFloat
let pvaluebool : Parser<_> = (ptrue <|> pfalse) |>> ValueBool
let pvaluestring : Parser<_> = ptext |>> ValueString

let pvalue' = choice [pvaluefloat; pvaluebool; pvaluestring]

// attribute parsers
let paria = 
    pcustom parianame ariaCtor

let pdata = 
    pcustom pdataname dataCtor

let palt = 
    pattribute "alt" ptext (AltAttribute >> Alt)

let pautoplay = 
    pattribute "autoplay" pboolean (AutoplayAttribute >> Autoplay)

let pclass = 
    pattribute "class" pclass' Class

let pheight = 
    pattribute "height" pfloat (ImageHeight >> Height)

let phref = 
    pattribute "href" purl (HrefAttribute >> Href)

let pid = 
    pattribute "id" pname (IdAttribute >> Id)

let ploop = 
    pattribute "loop" pboolean (LoopAttribute >> Loop)

let pmuted = 
    pattribute "muted" pboolean (MutedAttribute >> Muted)

let ppreload = 
    pattribute "preload" ppreload' (PreloadAttribute >> Preload)

let prole = 
    pattribute "role" (prole' |>> RoleAttribute |> many1) (Role)

let psrc = 
    pattribute "src" purl' (SrcAttribute >> Src)

let psrcset = 
    pattribute "srcset" (sepBy psrcsetimage (pchar ',')) SrcSet

let psizes = 
    pattribute "sizes" psizes'' Sizes

let pstyle = 
    pattribute "style" ptext (StyleAttribute >> Style)

let ptitle = 
    pattribute "title" ptext (TitleAttribute >> Title)

let ptype = 
    pattribute "type" ptype' (TypeAttribute >> Type)

let pvalue = 
    pbeginattribute "value" >>. pvalue' .>> pendattribute .>> spaces

let pwidth = 
    pattribute "width" pfloat (ImageWidth >> Width)
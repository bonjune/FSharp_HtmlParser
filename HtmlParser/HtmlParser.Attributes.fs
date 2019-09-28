module Attributes

open FParsec
open HtmlParser.Types

// attribute parser helpers
let pattribute name value constr : Parser<_> = 
    name >>. value .>> pstring "\"" .>> spaces |>> constr

let mapPstringChoice : (string list -> Parser<_>) = choice << (List.map pstring)

let pariavalue : Parser<_> = 
    mapPstringChoice ["label"; "haspopup"; "expanded"; "valuemin"; "valuemax"; "valuenow"; ]

let parianame : Parser<_> = 
    pstring "aria-" >>. pariavalue .>> pstring "=\""

let pattrname name : Parser<_> = pstring (name + "=\"")

let pbooleanvalue : Parser<_> = stringReturn "true" (Some true) <|> stringReturn "false" (Some false) <|> stringReturn "" None

let pcontentvalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_ \n,./?;:|!@#$%^&*()+=-" c || isLetter c || isDigit c)

let pfile : Parser<_> = 
    pipe2 (many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c))
          (pchar '.' >>. many1Satisfy isLetter)
          (fun n e -> ImageFile { Name = n; Extension = e })

let pidentifiervalue : Parser<_> = 
    many1Satisfy (fun c -> isAnyOf "_-" c || isLetter c || isDigit c) .>> spaces

let pdataname : Parser<_> = 
    pstring "data-" >>. pidentifiervalue .>> pstring "=\""

let ppreloadvalue : Parser<_> =
    (mapPstringChoice ["auto"; "none"; "metadata"]) <|> (stringReturn "" "auto")

let roleOptions = ["banner"; "button"; "complementary"; "contentinfo"; "definition"; "main"; "menuitem"; "menu"; "navigation"; "note"; "progressbar"; "search"]

let prolevalue : Parser<_> =
    mapPstringChoice roleOptions .>> spaces

let typeOptions = ["button"; "checkbox"; "color"; "datetime-local"; "date"; "email"; "file"; "hidden"; "image"; "month"; "number"; "password"; "radio"; "range"; "reset"; "search"; "submit"; "tel"; "text/javascript"; "text"; "time"; "url"; "video/mp4"; "week"]

let ptypevalue : Parser<_> =
    mapPstringChoice typeOptions

let purlvalue : Parser<_> =
    many1Satisfy (fun c -> isAnyOf "_,./?;:!@#$%^&*()+=-" c || isLetter c || isDigit c)

let purl : Parser<_> =
    pipe2 (mapPstringChoice ["http://"; "https://"; "#"; "/";]) purlvalue (fun h v -> h + v)

let psrcsetimage : Parser<_> =
    pipe2 (spaces >>. (purl |>> (Url >> ImageUrl) <|> pfile) .>> spaces) (pfloat .>> pchar 'w' |>> ImageWidth) (fun i w -> { Src = i; Width = w })

// attribute parsers
let palt = pattribute (pattrname "alt") (pcontentvalue |>> AltAttribute) Alt

let paria = pipe2 parianame 
                  (pcontentvalue .>> pstring "\"" .>> spaces)
                  (fun n c -> Aria { Name = n; Content = c })

let pautoplay = pattribute (pattrname "autoplay") pbooleanvalue (AutoplayAttribute >> Autoplay)

let pclass = pattribute (pattrname "class") (many1 (pidentifiervalue |>> ClassAttribute.create)) Class

let pdata = pipe2 pdataname
                  (pcontentvalue .>> pstring "\"" .>> spaces)
                  (fun n c -> Data { Name = n; Content = c })

let pheight = pattribute (pattrname "height") pfloat (ImageHeight >> Height)

let phref = pattribute (pattrname "href") purl (HrefAttribute >> Href)

let pid = pattribute (pattrname "id") pidentifiervalue (IdAttribute >> Id)

let ploop = pattribute (pattrname "loop") pbooleanvalue (LoopAttribute >> Loop)

let pmediawidth = mapPstringChoice ["em"; "px"; "vw";]

let pmediaconditional : Parser<_> = (stringReturn "max-width:" MaxWidth) <|> (stringReturn "min-width:" MinWidth)

let pmediacondition' = pipe2 (pmediaconditional .>> spaces) (pfloat .>> pmediawidth) (fun c w -> { Condition = c; Size = w })

let pmediacondition = between (pchar '(') (pchar ')') pmediacondition'

let pmuted = pattribute (pattrname "muted") pbooleanvalue (MutedAttribute >> Muted)

let ppreload = pattribute (pattrname "preload") ppreloadvalue (PreloadAttribute >> Preload)

let prole = pattribute (pattrname "role") (many1 (prolevalue |>> RoleAttribute)) Role

let psrc = pattribute (pattrname "src") (purlvalue) (SrcAttribute >> Src)

let psrcset = pattribute (pattrname "srcset") (sepBy psrcsetimage (pchar ',')) SrcSet

let pwidthslot = pfloat .>> pmediawidth

let psizes' = pipe2 (pmediacondition .>> spaces) (sepBy (pwidthslot |>> ImageWidth) (pchar ',' .>> spaces)) (fun m ws -> { Media = m; Widths = ws })

let psizes = pattribute (pattrname "sizes") psizes' Sizes

let ptitle = pattribute (pattrname "title") pcontentvalue (TitleAttribute >> Title)

let ptype = pattribute (pattrname "type") ptypevalue (TypeAttribute >> Type)

let pvalue' = (pfloat |>> ValueFloat <|> ((stringReturn "true" true <|> stringReturn "false" false) |>> ValueBool) <|> (pcontentvalue |>> ValueString))
let pvalue = pattrname "value" >>. pvalue' .>> pstring "\"" .>> spaces

let pwidth = pattribute (pattrname "width") pfloat (ImageWidth >> Width)
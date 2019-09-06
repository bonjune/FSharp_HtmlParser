module parser_playground

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

let test p str =
    match run p str with 
    | Success(result, _, _)   -> printfn "Success: %A" result 
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg

let str s = pstring s
let ws = spaces
let str_ws s = pstring s .>> ws
let float_ws : Parser<_> = pfloat .>> ws 

let between pBegin pEnd p = pBegin >>. p .>> pEnd
let betweenStrings s1 s2 p = p |> between (str s1) (str s2) 

let floatBetweenBrackets : Parser<_> = pfloat |> betweenStrings "[" "]"
let floatBetweenDoubleBrackets : Parser<_> = pfloat |> betweenStrings "[[" "]]"
let floatList : Parser<_> = str "[" >>. sepBy pfloat (str ",") .>> str "]"
let numberList = str_ws "[" >>. sepBy float_ws (str_ws ",") .>> str_ws "]"

let identifier : Parser<_> =
    let isIdentifierFirstChar c = isLetter c || c = '_'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws

let stringLiteral : Parser<_> =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                        | 'n' -> '\n'
                        | 'r' -> '\r'
                        | 't' -> '\t'
                        | c -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

let stringLiteral2 : Parser<_> =
    let normalCharSnippet = many1Satisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c -> string c )
    between (pstring "\"") (pstring "\"")
            (manyStrings (normalCharSnippet <|> escapedChar))

let stringLiteral3 : Parser<_> =
    let normalCharSnippet = manySatisfy (fun c -> c <> '\\' && c <> '"')
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> function
                                                            | 'n' -> "\n"
                                                            | 'r' -> "\r"
                                                            | 't' -> "\t"
                                                            | c -> string c )
    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedChar)

let product = pipe2 float_ws (str_ws "*" >>. float_ws)
                    (fun x y -> x * y)

type StringConstant = StringConstant of string * string

let stringConstant = pipe3 identifier (str_ws "=") stringLiteral
                            (fun id _ str -> StringConstant(id, str))

let boolean : Parser<_> = (stringReturn "true" true) <|> (stringReturn "false" false)

open FParsec

type Attribute = Attribute of string 

type HeaderTag = H1 | H2 | H3 | H4 | H5 | H6
type PhrasingContent = PhrasingContent of string 

type HtmlHeader = {
    HeaderTag : HeaderTag 
    Attribute : Attribute list option 
    PhrasingContent : PhrasingContent
    }

module Headers =

    let pheadertagopen : Parser<_> = 
        pstring "<h" >>. 
        anyOf "123456" |>> 
            (fun c -> match c with
                | '1' -> H1
                | '2' -> H2 
                | '3' -> H3 
                | '4' -> H4 
                | '5' -> H5
                | '6' -> H6 
                | unknown -> sprintf "Unknown header tag %c" unknown |> failwith) .>> pstring ">"

    let pheadertagclose : Parser<_> = 
        pstring "</h" >>. anyOf "123456" .>> pstring ">"

    let pphrasingcontent : Parser<_> = 
        many1Satisfy (fun c -> isAnyOf " !@#$%^&*()_+{}[],./?:;'\"" c || isLetter c || isDigit c)

    let pheaders = 
        pipe2 pheadertagopen pphrasingcontent (fun t c -> {HeaderTag = t; Attribute = None; PhrasingContent = PhrasingContent c}) .>> pheadertagclose
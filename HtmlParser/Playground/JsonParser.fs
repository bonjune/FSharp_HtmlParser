module JsonParser

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type Json = Jstring of string 
          | JNumber of float 
          | JBool of bool 
          | JNull
          | JList of Json list
          | JObject of Map<string, Json>

let jvalue, jvalueRef = createParserForwardedToRef<Json, unit>()

let stringLiteral = 
    let escape = anyOf "\"\\/bfnrt"
                 |>> function
                    | 'b' -> "b"
                    | 'f' -> "\u000C"
                    | 'n' -> "\n"
                    | 'r' -> "\r"
                    | 't' -> "\t"
                    | c   -> string c

    let unicodeEscape =
        let hex2int c = (int c &&& 15) + (int c >>> 6)*9

        pstring "u" >>. pipe4 hex hex hex hex (fun h3 h2 h1 h0 ->
                (hex2int h3)*4096 + (hex2int h2)*256 + (hex2int h1)*16 + hex2int h0
                |> char |> string
        )

    let escapedCharSnippet = pstring "\\" >>. (escape <|> unicodeEscape)
    let normalCharSnippet = manySatisfy (fun c -> c <> '"' && c <> '\\')

    between (pstring "\"") (pstring "\"")
            (stringsSepBy normalCharSnippet escapedCharSnippet)

let listBetweenStrings sOpen sClose pElement f =
    between (pstring sOpen) (pstring sClose)
            (spaces >>. sepBy (pElement .>> spaces) (pstring "," >>. spaces) |>> f)

let keyValue = stringLiteral .>>. (spaces >>. pstring ":" >>. spaces >>. jvalue)

let jnull : Parser<_> = stringReturn "null" JNull
let jtrue : Parser<_> = stringReturn "true" (JBool true)
let jfalse : Parser<_> = stringReturn "false" (JBool false)
let jnumber : Parser<_> = pfloat |>> JNumber
let jstring : Parser<_> = stringLiteral |>> Jstring
let jlist : Parser<_> = listBetweenStrings "[" "]" jvalue JList
let jobject : Parser<_> = listBetweenStrings "{" "}" keyValue (Map.ofList >> JObject)

do jvalueRef := choice [jobject
                        jlist
                        jstring
                        jnumber
                        jtrue
                        jfalse
                        jnull]

let json = spaces >>. jvalue .>> spaces .>> eof 


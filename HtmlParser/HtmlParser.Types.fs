namespace HtmlParser.Types

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type Attributes = 
    | Class of string list
    | Id of string 
    | Title of string
    | Href of string
    | Src of string
    | Alt of string
    | Width of float
    | Height of float
    | Data of string * string

type TagContent = 
    | Content of string
    | Body of Tag
    | Div of Tag

and Tag = {
    Attributes : Attributes list
    Content : TagContent list
    }
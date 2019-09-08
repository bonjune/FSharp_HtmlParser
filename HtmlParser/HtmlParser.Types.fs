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
    | Width of string
    | Height of string
    | Data of string * string
    | Role of string list
    | Aria of string * string 
    | Value of string 
    | Type of string 
    | Autoplay of string
    | Loop of string
    | Muted of string 
    | Preload of string

type TagContent = 
    | Content of string
    | Body of Tag
    | Div of Tag
    | ATag of Tag
    | Ul of Tag 
    | Li of Tag 
    | Video of Tag
    | Source of ScTag
    | Img of ScTag
    | H1 of Tag
    | H2 of Tag 
    | H3 of Tag 
    | H4 of Tag 
    | H5 of Tag 
    | H6 of Tag 
    | PTag of Tag

and Tag = {
    Attributes : Attributes list
    Content : TagContent list
    }

and ScTag = {
    Attributes : Attributes list
    }
namespace HtmlParser.Types

open FParsec

type Attributes = 
    | Alt of AltAttribute
    | Aria of AriaAttribute
    | Autoplay of AutoplayAttribute
    | Class of Result<ClassAttribute, string> list
    | Data of DataAttribute
    | Height of ImageHeight
    | Href of HrefAttribute
    | Id of IdAttribute
    | Loop of LoopAttribute
    | Muted of MutedAttribute
    | Preload of PreloadAttribute
    | Role of RoleAttribute list
    | Sizes of SizesAttribute
    | Src of SrcAttribute
    | SrcSet of SrcSetAttribute list
    | Title of TitleAttribute
    | Type of TypeAttribute
    | ValueString of string
    | ValueFloat of float
    | ValueBool of bool
    | Width of ImageWidth

type ElementContent = 
    | AElement of Element
    | Body of Element
    | Content of string
    | Div of Element
    | H1 of Element
    | H2 of Element 
    | H3 of Element 
    | H4 of Element 
    | H5 of Element 
    | H6 of Element
    | Img of ScElement
    | Li of Element 
    | PElement of Element
    | Source of ScElement
    | Ul of Element 
    | Video of Element

and Element = {
    Attributes : Attributes list
    Content : ElementContent list
    }

and ScElement = {
    Attributes : Attributes list
    }

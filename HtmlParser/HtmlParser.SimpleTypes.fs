namespace HtmlParser.Types

open System
open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

type AltAttribute = AltAttribute of string

type AriaAttribute = {
    Name : string
    Content : string 
    }

type AutoplayAttribute = AutoplayAttribute of bool option

type ClassAttribute = private ClassAttribute of string

type DataAttribute = {
    Name : string 
    Content : string 
    }

type ImageHeight = ImageHeight of float

type ImageWidth = ImageWidth of float

type HrefAttribute = HrefAttribute of string

type IdAttribute = IdAttribute of string

type LoopAttribute = LoopAttribute of bool option

type MutedAttribute = MutedAttribute of bool option

type PreloadAttribute = PreloadAttribute of string

type RoleAttribute = RoleAttribute of string

type MediaCondition = {
    Condition : string
    Size : float
    }

type SizesAttribute = {
    Media : MediaCondition
    Width : ImageWidth
    }

type SrcAttribute = SrcAttribute of string

type File = {
    Name : string
    Extension : string
    }

type Url = Url of string

type ImageSrc = ImageFile of File | ImageUrl of Url

type SrcSetAttribute = {
    Src : ImageSrc
    Width : ImageWidth
    }

type TitleAttribute = TitleAttribute of string

type TypeAttribute = TypeAttribute of string

type AttributeError =
    | ClassError of string

module ClassAttribute =
    
    let isValidChar char =
        char = '_' || char = '-' || Char.IsLetterOrDigit char

    let isValidFirstChar char = 
        char = '_' || char = '-' || Char.IsLetter char

    let isValidSecondChar first second =
        if first = '-' then second = '_' || Char.IsLetter second else isValidChar second

    let isVaildString str =
        List.fold (&&) true (List.map isValidChar str)

    let isValidClassName name =
        match Seq.toList name with 
        | [] -> false
        | (first::[]) -> isValidFirstChar first
        | (first::second::tail) -> 
            isValidFirstChar first && isValidSecondChar first second && isVaildString tail

    let create str =
        if isValidClassName str then
            Result.Ok (ClassAttribute str)
        else
            Result.Error str

    let value (ClassAttribute str) = str
        
namespace HtmlParser.Types

open FParsec

type UserState = unit
type Parser<'t> = Parser<'t, UserState>

// Global Attributes
type ClassName = ClassName of string
type ClassAtt = ClassAtt of ClassName list
type IdAtt = IdAtt of string
type TitleAtt = TitleAtt of string 

// Link Attributes
type HrefAtt = HrefAtt of string

// Image Attributes
type SrcAtt = SrcAtt of string
type WidthAtt = WidthAtt of float
type HeightAtt = HeightAtt of float 
type AltAtt = AltAtt of string

// Label Attributes
type ForAtt = ForAtt of string
type LabelAtt = LabelAtt of string 

type LangAtt = LangAtt of string 

// Input Attributes
type MaxAtt = MaxAtt of float
type MinAtt = MinAtt of float 
type MaxLengthAtt = MaxLengthAtt of int 
type MinLengthAtt = MinLengthAtt of int 
type NameAtt = NameAtt of string 
type RequiredAtt = RequiredAtt
type SelectedAtt = SelectedAtt 
type ValueAtt = ValueAtt of string 

type GlobalAtts = 
    | Class of ClassAtt 
    | Id of IdAtt 
    | Title of TitleAtt

type Tags = 
    | HtmlBody of HtmlBody

and HtmlBody = {
    Attributes : GlobalAtts list
    Tags : Tags list
    }
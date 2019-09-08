namespace HtmlParser.Attributes.Tests

open NUnit.Framework
open FParsec
open TestUtilities
open HtmlParser.Types
open Attributes

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ItShouldParseClassAttributeWithOneClassName () =
        let attribute = "class=\"hs-home\""
        let actual = 
            match run pclass attribute with 
            | Success(result, _, _)   -> result 
            | Failure(errorMsg, _, _) -> Class []

        let expected = Class [ "hs-home" ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseClassAttributeWithThreeClassNames () =
        let attribute = "class=\"hs-home   hs-content-id-10345721341 hs-site-page page \""
        let actual = test pclass attribute (Class [])
        let expected = 
            Class [ 
                    "hs-home"
                    "hs-content-id-10345721341"
                    "hs-site-page"
                    "page"
                    ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseIdAttribute () =
        let attribute = "id=\"hs_cos_wrapper_module_154623948344039\""
        let actual = test pid attribute (Id "")
        let expected = Id "hs_cos_wrapper_module_154623948344039"

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseTitleAttribute() =
        let attribute = "title=\"Download the Case Study\""
        let actual = test ptitle attribute (Title "")
        let expected = Title "Download the Case Study"

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDataAttribute() =
        let attribute = "data-widget-type=\"custom_widget\""
        let actual = test pdata attribute (Data ("", ""))
        let expected = Data ("data-widget-type", "custom_widget")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHrefAttribute() =
        let attribute = "href=\"https://www.callibrity.com/strategies/agile/\""
        let actual = test phref attribute (Href "")
        let expected = Href "https://www.callibrity.com/strategies/agile/"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseRoleAttribute() =
        let attribute = "role=\"menuitem menu navigation button\""
        let actual = test prole attribute (Role [])
        let expected = Role ["menuitem"; "menu"; "navigation"; "button"]

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAriaAttribute() =
        let attribute = "aria-label=\"Previous Slide\""
        let actual = test paria attribute (Aria ("", ""))
        let expected = Aria ("aria-label", "Previous Slide")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueAttribute() =
        let attribute = "value=\"true\""
        let actual = test pvalue attribute (Value "")
        let expected = Value "true"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseTypeAttribute() =
        let attribute = "type=\"text/javascript\""
        let actual = test ptype attribute (Type "")
        let expected = Type "text/javascript"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAutoplayAttribute() =
        let attribute = "autoplay=\"false\""
        let actual = test pautoplay attribute (Autoplay "")
        let expected = Autoplay "false"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseLoopAttribute() =
        let attribute = "loop=\"true\""
        let actual = test ploop attribute (Loop "")
        let expected = Loop "true"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseMutedAttribute() =
        let attribute = "muted=\"true\""
        let actual = test pmuted attribute (Muted "")
        let expected = Muted "true"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParsePreloadAttribute() =
        let attribute = "preload=\"auto\""
        let actual = test ppreload attribute (Preload "")
        let expected = Preload "auto"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseEmptyPreloadAttribute() =
        let attribute = "preload=\"\""
        let actual = test ppreload attribute (Preload "")
        let expected = Preload "auto"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseSrcAttribute() =
        let attribute = "src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\""
        let actual = test psrc attribute (Src "")
        let expected = Src "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4"

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseAltAttribute() =
        let attribute = "alt=\"Founded by Developers for Developers\""
        let actual = test palt attribute (Alt "")
        let expected = Alt "Founded by Developers for Developers"

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHeightAttribute() =
        let attribute = "height=\"736\""
        let actual = test pheight attribute (Height "")
        let expected = Height "736"

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseWidthAttribute() =
        let attribute = "width=\"1280\""
        let actual = test pwidth attribute (Width "")
        let expected = Width "1280"

        Assert.AreEqual(expected, actual)

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

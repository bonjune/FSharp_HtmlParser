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
            | Failure(errorMsg, _, _) -> Class (ClassAtt [])

        let expected = Class (ClassAtt [ ClassName "hs-home" ])

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseClassAttributeWithThreeClassNames () =
        let attribute = "class=\"hs-home   hs-content-id-10345721341 hs-site-page page \""
        let actual = 
            match run pclass attribute with 
            | Success(result, _, _)   -> result 
            | Failure(_, _, _) -> Class (ClassAtt [])

        let expected = 
            Class (ClassAtt [ 
                    ClassName "hs-home"
                    ClassName "hs-content-id-10345721341"
                    ClassName "hs-site-page"
                    ClassName "page"
                    ])

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseIdAttribute () =
        let attribute = "id=\"hs_cos_wrapper_module_154623948344039\""
        let actual = 
            match run pid attribute with 
            | Success(result, _, _)   -> result 
            | Failure(errorMsg, _, _) -> Id (IdAtt "")

        let expected = Id (IdAtt "hs_cos_wrapper_module_154623948344039")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseTitleAttribute() =
        let attribute = "title=\"Download the Case Study\""
        let actual = 
            match run ptitle attribute with 
            | Success(result, _, _)   -> result 
            | Failure(errorMsg, _, _) -> Title (TitleAtt "")

        let expected = Title (TitleAtt "Download the Case Study")

        Assert.AreEqual(expected, actual)

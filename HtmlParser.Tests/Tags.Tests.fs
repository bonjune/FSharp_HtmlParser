namespace HtmlParser.Tags.Tests

open NUnit.Framework
open FParsec
open TestUtilities
open HtmlParser.Types
open Attributes
open Tags

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAttribute () =
        let body = "<body class=\"hs-home\"></body>"
        let actual =
            match run pbody body with
            | Success(result, _, _)   -> result 
            | Failure(_, _, _) -> { Attributes = []; Tags = [] }

        let expected = 
            { Attributes = [
                Class(ClassAtt [ClassName "hs-home"]) 
                ];
            Tags = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAttributes () =
        let body = "<body class=\"hs-home\" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual =
            match run pbody body with
            | Success(result, _, _)   -> result 
            | Failure(_, _, _) -> { Attributes = []; Tags = [] }

        let expected = 
            { Attributes = [
                Class(ClassAtt [ClassName "hs-home"]) 
                Id(IdAtt "hs_cos_wrapper_module_154623948344039") 
                ]; 
            Tags = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAndTitleAttributes () =
        let body = "<body title=\"Download the Case Study\" class=\"hs-home   hs-content-id-10345721341 hs-site-page page \" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual =
            match run pbody body with
            | Success(result, _, _)   -> result 
            | Failure(_, _, _) -> { Attributes = []; Tags = [] }

        let expected = 
            { Attributes = [
                Title(TitleAtt "Download the Case Study")
                Class(ClassAtt [
                    ClassName "hs-home"
                    ClassName "hs-content-id-10345721341"
                    ClassName "hs-site-page"
                    ClassName "page"
                    ]) 
                Id(IdAtt "hs_cos_wrapper_module_154623948344039") 
                ]; 
            Tags = [] 
            }

        Assert.AreEqual(expected, actual)


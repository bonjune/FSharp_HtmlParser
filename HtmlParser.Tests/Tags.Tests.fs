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
        let tag = "<body class=\"hs-home\"></body>"
        let actual = test pbody tag (tagErr Body)
        let expected = 
            Body { 
                Attributes = [
                    Class ["hs-home"]
                    ];
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAttributes () =
        let tag = "<body class=\"hs-home\" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual = test pbody tag (tagErr Body)
        let expected = 
            Body { 
                Attributes = [
                    Class ["hs-home"] 
                    Id "hs_cos_wrapper_module_154623948344039"
                    ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAndTitleAttributes () =
        let tag = "<body title=\"Download the Case Study\" class=\"hs-home   hs-content-id-10345721341 hs-site-page page \" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual = test pbody tag (tagErr Body)
        let expected = 
            Body { 
                Attributes = [
                    Title "Download the Case Study"
                    Class [
                        "hs-home"
                        "hs-content-id-10345721341"
                        "hs-site-page"
                        "page"
                        ]
                    Id "hs_cos_wrapper_module_154623948344039"
                ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDivTagWithClassAttribute() =
        let tag = "<div class=\"row-fluid \"></div>"
        let actual = test pdiv tag (tagErr Div)
        let expected = 
            Div { 
                Attributes = [ Class ["row-fluid"] ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseNestedDivTags() =
        let tags = "<div><div></div></div>"
        let actual = test pdiv tags (tagErr Div)
        let expected =
            Div {
                Attributes = []
                Content = [
                    Div {
                        Attributes = []
                        Content = []
                        }
                    ]
                }

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseDivTagInsideBodyTag() =
        let tags = "<body><div></div></body>"
        let actual = test pbody tags (tagErr Body)
        let expected =
            Body {
                Attributes = []
                Content = [
                    Div {
                        Attributes = []
                        Content = []
                        }
                    ]
                }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDivWithTextContent() =
        let tag = "<div>Content</div>"
        let actual = test pdiv tag (tagErr Div)
        let expected =
            Div {
                Attributes = []
                Content = [Content "Content"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseATag() =
        let tag = "<a id=\"cta_button_4021173_27313a4a-55fc-45c6-8a64-a6e216314352\" class=\"cta_button \" href=\"https://www.callibrity.com\" title=\"Download the Case Study\">Download the Case Study</a>"
        let actual = test patag tag (tagErr ATag)
        let expected =
            ATag {
                Attributes = [
                    Id "cta_button_4021173_27313a4a-55fc-45c6-8a64-a6e216314352";
                    Class ["cta_button"];
                    Href "https://www.callibrity.com";
                    Title "Download the Case Study"
                ]
                Content = [Content "Download the Case Study"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseUlTagWithLiTags() =
        let tag = "<ul><li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/agile/\" role=\"menuitem\">Agile</a></li>" +
                    "<li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/cloud\" role=\"menuitem\">Cloud</a></li></ul>"
        let actual = test pul tag (tagErr Ul)
        let aOne = 
            ATag {
                Attributes = [
                    Href "https://www.callibrity.com/strategies/agile/";
                    Role ["menuitem"]
                ]
                Content = [Content "Agile"]}
        let liOne = 
            Li {
                Attributes = [Class ["hs-menu-item"; "hs-menu-depth-1"]]
                Content = [aOne]
            }
        let aTwo = 
            ATag {
                Attributes = [
                    Href "https://www.callibrity.com/strategies/cloud";
                    Role ["menuitem"]
                ]
                Content = [Content "Cloud"]}
        let liTwo = 
            Li {
                Attributes = [Class ["hs-menu-item"; "hs-menu-depth-1"]]
                Content = [aTwo]
            }
        let expected =
            Ul {
                Attributes = []
                Content = [liOne; liTwo]
            }

        Assert.AreEqual(expected, actual)
                    


﻿namespace HtmlParser.Tags.Tests

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

    [<Test>]
    member this.ItShouldParseSourceTag() =
        let tag = "<source src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\" type=\"video/mp4\"/>"
        let actual = test psource tag (scTagErr Source)

        let srcAttr = Src "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4"
        let typeAttr = Type "video/mp4"
        let expected =
            Source { Attributes = [srcAttr; typeAttr] }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseVideoTag() =
        let tag = "<video id=\"bg-video\" class=\"hidden-phone img-responsive\" autoplay=\"\" loop=\"\" muted=\"\" preload=\"metadata\">" +
                    "<source src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\" type=\"video/mp4\"/></video>"
        let actual = test pvideo tag (tagErr Video)

        let srcAttr = Src "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4"
        let typeAttr = Type "video/mp4"
        let srcTag =
            Source { Attributes = [srcAttr; typeAttr] }
        let expected =
            Video {
                Attributes = [
                    Id "bg-video"
                    Class ["hidden-phone"; "img-responsive"] 
                    Autoplay "" 
                    Loop ""
                    Muted "" 
                    Preload "metadata" 
                ]
                Content = [srcTag]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseImgTag() =
        let tag = "<img class=\"visible-phone\" src=\"https://www.callibrity.com/hs-fs/hubfs/vlcsnap-error357.png?width=1280&amp;height=736&amp;name=vlcsnap-error357.png\" alt=\"Founded by Developers for Developers\" width=\"1280\" height=\"736\"/>"
        let actual = test pimg tag (scTagErr Img)
        let expected =
            Img {
                Attributes = [
                    Class ["visible-phone"]
                    Src "https://www.callibrity.com/hs-fs/hubfs/vlcsnap-error357.png?width=1280&amp;height=736&amp;name=vlcsnap-error357.png"
                    Alt "Founded by Developers for Developers"
                    Width "1280"
                    Height "736"
                ]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseH1Tag() =
        let tag = "<h1>True software craftsmanship, real business results</h1>"
        let actual = test ph1 tag (tagErr H1)
        let expected =
            H1 {
                Attributes = []
                Content = [Content "True software craftsmanship, real business results"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseH2Tag() =
        let tag = "<h2>Outperform financially</h2>"
        let actual = test ph2 tag (tagErr H2)
        let expected =
            H2 {
                Attributes = []
                Content = [Content "Outperform financially"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParsePTag() =
        let tag = "<p>When growing your business, being first matters. Speed to market + agility = peak financial performance.</p>"
        let actual = test pptag tag (tagErr PTag)
        let expected =
            PTag {
                Attributes = []
                Content = [Content "When growing your business, being first matters. Speed to market + agility = peak financial performance."]
            }

        Assert.AreEqual(expected, actual)
namespace HtmlParser.Elements.Tests

open NUnit.Framework
open FParsec
open TestUtilities
open HtmlParser.Types
open Attributes
open Elements

[<TestFixture>]
type TestClass () =

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAttribute () =
        let element = "<body class=\"hs-home\"></body>"
        let actual = elementTest pbody element
        let expected = 
            Body { 
                Attributes = [
                    Class [ ClassAttribute.create "hs-home"]
                    ];
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAttributes () =
        let element = "<body class=\"hs-home\" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual = elementTest pbody element
        let expected = 
            Body { 
                Attributes = [
                    Class [ClassAttribute.create "hs-home"] 
                    Id (IdAttribute "hs_cos_wrapper_module_154623948344039")
                    ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseBodyTagWithClassAndIdAndTitleAttributes () =
        let element = "<body title=\"Download the Case Study\" class=\"hs-home   hs-content-id-10345721341 hs-site-page page \" id=\"hs_cos_wrapper_module_154623948344039\"></body>"
        let actual = elementTest pbody element
        let expected = 
            Body { 
                Attributes = [
                    Title (TitleAttribute "Download the Case Study")
                    Class [
                        ClassAttribute.create "hs-home"
                        ClassAttribute.create "hs-content-id-10345721341"
                        ClassAttribute.create "hs-site-page"
                        ClassAttribute.create "page"
                        ]
                    Id (IdAttribute "hs_cos_wrapper_module_154623948344039")
                ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDivTagWithClassAttribute() =
        let element = "<div class=\"row-fluid \"></div>"
        let actual = elementTest pdiv element
        let expected = 
            Div { 
                Attributes = [ Class [ClassAttribute.create "row-fluid"] ]; 
                Content = [] 
            }

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseNestedDivTags() =
        let elements = "<div><div></div></div>"
        let actual = elementTest pdiv elements
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
        let elements = "<body><div></div></body>"
        let actual = elementTest pbody elements
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
        let element = "<div>Content</div>"
        let actual = elementTest pdiv element
        let expected =
            Div {
                Attributes = []
                Content = [Content "Content"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseATag() =
        let element = "<a id=\"cta_button_4021173_27313a4a-55fc-45c6-8a64-a6e216314352\" class=\"cta_button \" href=\"https://www.callibrity.com\" title=\"Download the Case Study\">Download the Case Study</a>"
        let actual = elementTest paelement element
        let expected =
            AElement {
                Attributes = [
                    Id (IdAttribute "cta_button_4021173_27313a4a-55fc-45c6-8a64-a6e216314352")
                    Class [ClassAttribute.create "cta_button"]
                    Href (HrefAttribute "https://www.callibrity.com")
                    Title (TitleAttribute "Download the Case Study")
                ]
                Content = [Content "Download the Case Study"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseUlTagWithLiTags() =
        let element = "<ul><li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/agile/\" role=\"menuitem\">Agile</a></li>" +
                      "<li class=\"hs-menu-item hs-menu-depth-1\"><a href=\"https://www.callibrity.com/strategies/cloud\" role=\"menuitem\">Cloud</a></li></ul>"
        let actual = elementTest pul element
        let aOne = 
            AElement {
                Attributes = [
                    Href (HrefAttribute "https://www.callibrity.com/strategies/agile/")
                    Role [RoleAttribute "menuitem"]
                ]
                Content = [Content "Agile"]}
        let liOne = 
            Li {
                Attributes = [Class [ClassAttribute.create "hs-menu-item"; ClassAttribute.create "hs-menu-depth-1"]]
                Content = [aOne]
            }
        let aTwo = 
            AElement {
                Attributes = [
                    Href (HrefAttribute "https://www.callibrity.com/strategies/cloud")
                    Role [RoleAttribute "menuitem"]
                ]
                Content = [Content "Cloud"]}
        let liTwo = 
            Li {
                Attributes = [Class [ClassAttribute.create "hs-menu-item"; ClassAttribute.create "hs-menu-depth-1"]]
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
        let element = "<source src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\" type=\"video/mp4\"/>"
        let actual = elementTest psource element

        let srcAttr = Src (SrcAttribute "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4")
        let typeAttr = Type (TypeAttribute "video/mp4")
        let expected =
            Source { Attributes = [srcAttr; typeAttr] }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseVideoTag() =
        let element = "<video id=\"bg-video\" class=\"hidden-phone img-responsive\" autoplay=\"\" loop=\"\" muted=\"\" preload=\"metadata\">" +
                      "<source src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\" type=\"video/mp4\"/></video>"
        let actual = elementTest pvideo element

        let srcAttr = Src (SrcAttribute "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4")
        let typeAttr = Type (TypeAttribute "video/mp4")
        let srcTag =
            Source { Attributes = [srcAttr; typeAttr] }
        let expected =
            Video {
                Attributes = [
                    Id (IdAttribute "bg-video")
                    Class [ClassAttribute.create "hidden-phone"; ClassAttribute.create "img-responsive"] 
                    Autoplay (AutoplayAttribute None )
                    Loop (LoopAttribute None)
                    Muted (MutedAttribute None)
                    Preload (PreloadAttribute "metadata" )
                ]
                Content = [srcTag]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseImgTag() =
        let element = "<img class=\"visible-phone\" src=\"https://www.callibrity.com/hs-fs/hubfs/vlcsnap-error357.png?width=1280&amp;height=736&amp;name=vlcsnap-error357.png\" alt=\"Founded by Developers for Developers\" width=\"1280\" height=\"736\"/>"
        let actual = elementTest pimg element
        let expected =
            Img {
                Attributes = [
                    Class [ClassAttribute.create "visible-phone"]
                    Src (SrcAttribute "https://www.callibrity.com/hs-fs/hubfs/vlcsnap-error357.png?width=1280&amp;height=736&amp;name=vlcsnap-error357.png")
                    Alt (AltAttribute "Founded by Developers for Developers")
                    Width (ImageWidth 1280.0)
                    Height (ImageHeight 736.0)
                ]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseH1Tag() =
        let element = "<h1>True software craftsmanship, real business results</h1>"
        let actual = elementTest ph1 element
        let expected =
            H1 {
                Attributes = []
                Content = [Content "True software craftsmanship, real business results"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseH2Tag() =
        let element = "<h2>Outperform financially</h2>"
        let actual = elementTest ph2 element
        let expected =
            H2 {
                Attributes = []
                Content = [Content "Outperform financially"]
            }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParsePTag() =
        let element = "<p>When growing your business, being first matters. Speed to market + agility = peak financial performance.</p>"
        let actual = elementTest ppelement element
        let expected =
            PElement {
                Attributes = []
                Content = [Content "When growing your business, being first matters. Speed to market + agility = peak financial performance."]
            }

        Assert.AreEqual(expected, actual)
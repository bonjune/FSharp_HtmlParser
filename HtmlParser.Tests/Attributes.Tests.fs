namespace HtmlParser.Attributes.Tests

open NUnit.Framework
open FParsec
open TestUtilities
open HtmlParser.Types
open Attributes

[<TestFixture>]
type TestClass () =
    [<Test>]
    member this.IsValidClassAttributeName () =
        let expected = 
            match ClassAttribute.create "hs-home" with
            | Result.Ok(success) -> ClassAttribute.value success
            | Result.Error(err) -> err + " is not a valid class name"

        Assert.AreEqual("hs-home", expected)

    [<Test>]
    member this.IsNotValidClassAttributeName () =
        let expected = 
            match ClassAttribute.create "@hs-home" with
            | Result.Ok(success) -> ClassAttribute.value success
            | Result.Error(err) -> err + " is not a valid class name"

        Assert.AreEqual("@hs-home is not a valid class name", expected)

    [<Test>]
    member this.IsNotValidSecondCharClassAttributeName () =
        let expected = 
            match ClassAttribute.create "-8hs-home" with
            | Result.Ok(success) -> ClassAttribute.value success
            | Result.Error(err) -> err + " is not a valid class name"

        Assert.AreEqual("-8hs-home is not a valid class name", expected)


    [<Test>]
    member this.ItShouldParseClassAttributeWithOneClassName () =
        let attribute = "class=\"hs-home\""
        let actual = attributeTest pclass attribute

        let expected = Class [ ClassAttribute.create "hs-home" ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseEmptyClassAttribute () =
        let attribute = "class=\"\""
        let actual = attributeTest pclass attribute 
        let expected = Class []

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseClassAttributeWithThreeClassNames () =
        let attribute = "class=\"hs-home   hs-content-id-10345721341 hs-site-page page \""
        let actual = attributeTest pclass attribute
        let expected = 
            Class [ 
                    ClassAttribute.create "hs-home"
                    ClassAttribute.create "hs-content-id-10345721341"
                    ClassAttribute.create "hs-site-page"
                    ClassAttribute.create "page"
                    ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseIdAttribute () =
        let attribute = "id=\"hs_cos_wrapper_module_154623948344039\""
        let actual = attributeTest pid attribute
        let expected = Id (IdAttribute "hs_cos_wrapper_module_154623948344039")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseTitleAttribute () =
        let attribute = "title=\"Download the Case Study\""
        let actual = attributeTest ptitle attribute
        let expected = Title (TitleAttribute "Download the Case Study")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDataAttribute () =
        let attribute = "data-widget-type=\"custom_widget\""
        let actual = attributeTest pdata attribute
        let expected = Data ({ Name = "widget-type"; Content = "custom_widget" })

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHrefAttribute () =
        let attribute = "href=\"https://www.callibrity.com/strategies/agile/\""
        let actual = attributeTest phref attribute
        let expected = Href (HrefAttribute "https://www.callibrity.com/strategies/agile/")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseRoleAttribute () =
        let attribute = "role=\"menuitem menu navigation button\""
        let actual = attributeTest prole attribute
        let expected = 
            Role [
                RoleAttribute "menuitem" 
                RoleAttribute "menu" 
                RoleAttribute "navigation" 
                RoleAttribute "button"
                ]

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAriaAttribute () =
        let attribute = "aria-label=\"Previous Slide\""
        let actual = attributeTest paria attribute
        let expected = Aria { Name = "label"; Content = "Previous Slide" }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueBoolAttribute () =
        let attribute = "value=\"true\""
        let actual = attributeTest pvalue attribute
        let expected = ValueBool true

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueFloatAttribute () =
        let attribute = "value=\"17.5\""
        let actual = attributeTest pvalue attribute
        let expected = ValueFloat 17.5

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueStringAttribute () =
        let attribute = "value=\"some value\""
        let actual = attributeTest pvalue attribute
        let expected = ValueString "some value"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseTypeAttribute () =
        let attribute = "type=\"text/javascript\""
        let actual = attributeTest ptype attribute
        let expected = Type (TypeAttribute "text/javascript")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAutoplayAttribute () =
        let attribute = "autoplay=\"false\""
        let actual = attributeTest pautoplay attribute
        let expected = Autoplay (AutoplayAttribute (Some false))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseLoopAttribute () =
        let attribute = "loop=\"true\""
        let actual = attributeTest ploop attribute
        let expected = Loop (LoopAttribute (Some true))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseMutedAttribute () =
        let attribute = "muted=\"true\""
        let actual = attributeTest pmuted attribute
        let expected = Muted (MutedAttribute (Some true))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParsePreloadAttribute () =
        let attribute = "preload=\"auto\""
        let actual = attributeTest ppreload attribute
        let expected = Preload (PreloadAttribute "auto")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseEmptyPreloadAttribute () =
        let attribute = "preload=\"\""
        let actual = attributeTest ppreload attribute
        let expected = Preload (PreloadAttribute "auto")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseSrcAttribute () =
        let attribute = "src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\""
        let actual = attributeTest psrc attribute
        let expected = Src (SrcAttribute "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseAltAttribute () =
        let attribute = "alt=\"Founded by Developers for Developers\""
        let actual = attributeTest palt attribute
        let expected = Alt (AltAttribute "Founded by Developers for Developers")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHeightAttribute () =
        let attribute = "height=\"736\""
        let actual = attributeTest pheight attribute
        let expected = Height (ImageHeight 736.0)

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseWidthAttribute () =
        let attribute = "width=\"1280\""
        let actual = attributeTest pwidth attribute
        let expected = Width (ImageWidth 1280.0)

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseSrcSetAttribute () =
        let attribute = 
            "srcset=\"
            image1.png    640w,
            image2.png  1280w,
            image3.png 1920w,
            https://www.callibrity.com/image4.png 2560w,
            https://www.callibrity.com/image5.png 3200w,
            image6.png 3840w\""
        let actual = attributeTest psrcset attribute
        let expected = SrcSet [
            { Src = ImageFile { Name = "image1"; Extension = "png" }
              Width = ImageWidth 640.0 }; 
            { Src = ImageFile { Name = "image2"; Extension = "png" }
              Width = ImageWidth 1280.0 };
            { Src = ImageFile { Name = "image3"; Extension = "png" }
              Width = ImageWidth 1920.0 }; 
            { Src = ImageUrl (Url "https://www.callibrity.com/image4.png")
              Width = ImageWidth 2560.0 };
            { Src = ImageUrl (Url "https://www.callibrity.com/image5.png")
              Width = ImageWidth 3200.0 }; 
            { Src = ImageFile { Name = "image6"; Extension = "png" }
              Width = ImageWidth 3840.0 }
            ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseSizesAttribute () =
        let attribute = "sizes=\"(max-width: 162px) 100vw, 162px\""
        let actual = attributeTest psizes attribute
        let sizes = {
            Media = {
                Condition = MaxWidth
                Size = 162.0
                }
            Widths = [ImageWidth 100.0; ImageWidth 162.0]
            }
        let expected = Sizes sizes

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseStyleAttribute () =
        let attribute = "style=\"text-align: center;\""
        let actual = attributeTest pstyle attribute
        let expected = Style (StyleAttribute "text-align: center;")

        Assert.AreEqual(expected, actual)


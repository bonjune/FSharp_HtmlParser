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
        let actual = 
            match run pclass attribute with 
            | Success(result, _, _)   -> result 
            | Failure(errorMsg, _, _) -> Class []

        let expected = Class [ ClassAttribute.create "hs-home" ]

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseClassAttributeWithThreeClassNames () =
        let attribute = "class=\"hs-home   hs-content-id-10345721341 hs-site-page page \""
        let actual = test pclass attribute (Class [])
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
        let actual = test pid attribute (Id (IdAttribute ""))
        let expected = Id (IdAttribute "hs_cos_wrapper_module_154623948344039")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseTitleAttribute() =
        let attribute = "title=\"Download the Case Study\""
        let actual = test ptitle attribute (Title (TitleAttribute ""))
        let expected = Title (TitleAttribute "Download the Case Study")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseDataAttribute() =
        let attribute = "data-widget-type=\"custom_widget\""
        let actual = test pdata attribute (Data { Name = ""; Content = "" })
        let expected = Data ({ Name = "widget-type"; Content = "custom_widget" })

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHrefAttribute() =
        let attribute = "href=\"https://www.callibrity.com/strategies/agile/\""
        let actual = test phref attribute (Href (HrefAttribute ""))
        let expected = Href (HrefAttribute "https://www.callibrity.com/strategies/agile/")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseRoleAttribute() =
        let attribute = "role=\"menuitem menu navigation button\""
        let actual = test prole attribute (Role [])
        let expected = 
            Role [
                RoleAttribute "menuitem" 
                RoleAttribute "menu" 
                RoleAttribute "navigation" 
                RoleAttribute "button"
                ]

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAriaAttribute() =
        let attribute = "aria-label=\"Previous Slide\""
        let actual = test paria attribute (Aria { Name = ""; Content = "" })
        let expected = Aria { Name = "label"; Content = "Previous Slide" }

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueBoolAttribute() =
        let attribute = "value=\"true\""
        let actual = test pvalue attribute (ValueBool false)
        let expected = ValueBool true

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueFloatAttribute() =
        let attribute = "value=\"17.5\""
        let actual = test pvalue attribute (ValueFloat 0.0)
        let expected = ValueFloat 17.5

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseValueStringAttribute() =
        let attribute = "value=\"some value\""
        let actual = test pvalue attribute (ValueString "")
        let expected = ValueString "some value"

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseTypeAttribute() =
        let attribute = "type=\"text/javascript\""
        let actual = test ptype attribute (Type (TypeAttribute ""))
        let expected = Type (TypeAttribute "text/javascript")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseAutoplayAttribute() =
        let attribute = "autoplay=\"false\""
        let actual = test pautoplay attribute (Autoplay (AutoplayAttribute None))
        let expected = Autoplay (AutoplayAttribute (Some false))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseLoopAttribute() =
        let attribute = "loop=\"true\""
        let actual = test ploop attribute (Loop (LoopAttribute None))
        let expected = Loop (LoopAttribute (Some true))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseMutedAttribute() =
        let attribute = "muted=\"true\""
        let actual = test pmuted attribute (Muted (MutedAttribute None))
        let expected = Muted (MutedAttribute (Some true))

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParsePreloadAttribute() =
        let attribute = "preload=\"auto\""
        let actual = test ppreload attribute (Preload (PreloadAttribute ""))
        let expected = Preload (PreloadAttribute "auto")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseEmptyPreloadAttribute() =
        let attribute = "preload=\"\""
        let actual = test ppreload attribute (Preload (PreloadAttribute ""))
        let expected = Preload (PreloadAttribute "auto")

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseSrcAttribute() =
        let attribute = "src=\"https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4\""
        let actual = test psrc attribute (Src (SrcAttribute ""))
        let expected = Src (SrcAttribute "https://cdn2.hubspot.net/hubfs/4021173/Callibrity_December2018%20Theme/Videos/callibrity-movie.mp4")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseAltAttribute() =
        let attribute = "alt=\"Founded by Developers for Developers\""
        let actual = test palt attribute (Alt (AltAttribute""))
        let expected = Alt (AltAttribute "Founded by Developers for Developers")

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseHeightAttribute() =
        let attribute = "height=\"736\""
        let actual = test pheight attribute (Height (ImageHeight 0.0))
        let expected = Height (ImageHeight 736.0)

        Assert.AreEqual(expected, actual)

    [<Test>]
    member this.ItShouldParseWidthAttribute() =
        let attribute = "width=\"1280\""
        let actual = test pwidth attribute (Width (ImageWidth 0.0))
        let expected = Width (ImageWidth 1280.0)

        Assert.AreEqual(expected, actual)

    [<Test>] 
    member this.ItShouldParseSrcSetAttribute() =
        let attribute = 
            "srcset=\"
            image1.png    640w,
            image2.png  1280w,
            image3.png 1920w,
            https://www.callibrity.com/image4.png 2560w,
            https://www.callibrity.com/image5.png 3200w,
            image6.png 3840w\""
        let actual = test psrcset attribute (SrcSet [])
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
    member this.ItShouldParseSizesAttribute() =

        Assert.Pass()


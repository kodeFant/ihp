{-|
Module: IHP.HSX.QQSpec
Copyright: (c) digitally induced GmbH, 2020
-}
module IHP.HSX.ParserSpec where

import Test.Hspec
import Prelude
import IHP.HSX.Parser
import qualified Text.Megaparsec as Megaparsec
import qualified Text.Megaparsec.Error as Megaparsec
import qualified "template-haskell" Language.Haskell.TH as TH
import qualified "template-haskell" Language.Haskell.TH.Syntax as TH

tests = do
    let position = Megaparsec.SourcePos "" (Megaparsec.mkPos 1) (Megaparsec.mkPos 1)
    let extensions = []
    
    describe "HSX Parser" do
        let checkMarkup = True
        it "should fail on invalid html tags" do
            let errorText = "1:13:\n  |\n1 | <myinvalidel>\n  |             ^\nInvalid tag name: myinvalidel\n"
            let (Left error) = parseHsx checkMarkup position extensions "<myinvalidel>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should fail on invalid attribute names" do
            let errorText = "1:23:\n  |\n1 | <div invalid-attribute=\"test\">\n  |                       ^\nInvalid attribute name: invalid-attribute\n"
            let (Left error) = parseHsx checkMarkup position extensions "<div invalid-attribute=\"test\">"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should fail on unmatched tags" do
            let errorText = "1:7:\n  |\n1 | <div></span>\n  |       ^\nunexpected '/'\nexpecting \"</div>\", identifier, or white space\n"
            let (Left error) = parseHsx checkMarkup position extensions "<div></span>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should parse a closing tag with spaces" do
            let p = parseHsx checkMarkup position extensions "<div></div >"
            p `shouldBe` (Right (Children [Node "div" [] [] False]))

        it "should strip spaces around nodes" do
            let p = parseHsx checkMarkup position extensions "<div> <span> </span> </div>"
            p `shouldBe` (Right (Children [Node "div" [] [Node "span" [] [] False] False]))

        it "should strip spaces after self closing tags" do
            let p = parseHsx checkMarkup position extensions "<head>{\"meta\"}\n\n                        <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"></head>"
            p `shouldBe` (Right (Children [Node "head" [] [SplicedNode (TH.LitE (TH.StringL "meta")),Node "link" [StaticAttribute "rel" (TextValue "stylesheet"),StaticAttribute "href" (TextValue "/vendor/bootstrap.min.css")] [] True] False]))

        it "should not strip spaces in a text node" do
            let p = parseHsx checkMarkup position extensions " Hello World "
            p `shouldBe` (Right (Children [TextNode "Hello World"]))

        it "should deal with variables in text nodes" do
            let p = parseHsx checkMarkup position extensions "<div>\n    Hello {\"name\"}! \n</div>"
            p `shouldBe`  (Right (Children [Node "div" [] [TextNode "Hello ",SplicedNode (TH.LitE (TH.StringL "name")),TextNode "!"] False]))

        it "should parse self closing tags with spaces around it" do
            let p = parseHsx checkMarkup position extensions " <div/> "
            p `shouldBe`  (Right (Children [Node "div" [] [] False]))

        it "should collapse spaces" do
            let p = parseHsx checkMarkup position extensions "\n    Hello\n    World\n    !    "
            p `shouldBe`  (Right (Children [TextNode "Hello World !"]))
        
        it "should parse spread values" do
            let p = parseHsx checkMarkup position extensions "<div {...variables}/>"
            -- We cannot easily construct the @VarE variables@ expression, therefore we use show here for comparison
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should parse spread values with a space" do
            -- See https://github.com/digitallyinduced/ihp/issues/1588
            let p = parseHsx checkMarkup position extensions "<div { ...variables }/>"
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should accept underscores in data attributes" do
            let p = parseHsx checkMarkup position extensions "<div data-client_id=\"test\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "data-client_id" (TextValue "test")] [] False]))

        it "should accept doctype" do
            let p = parseHsx checkMarkup position extensions "<!DOCTYPE html><html lang=\"en\"><body>hello</body></html>"
            p `shouldBe` (Right (Children [Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True, Node "html" [StaticAttribute "lang" (TextValue "en")] [Node "body" [] [TextNode "hello"] False] False]))

    describe "uncheckedHsx" do
        let checkMarkup = False
        it "should not check markup" do
            let p = parseHsx False position extensions "<invalid-tag invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "invalid-tag" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))
        
        it "should not check attribute names" do
            let p = parseHsx False position extensions "<div invalid-attribute=\"invalid\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "invalid-attribute" (TextValue "invalid")] [] False]))

        it "should fail on unmatched tags" do
            let errorText = "1:7:\n  |\n1 | <div></span>\n  |       ^\nunexpected '/'\nexpecting \"</div>\", identifier, or white space\n"
            let (Left error) = parseHsx checkMarkup position extensions "<div></span>"
            (Megaparsec.errorBundlePretty error) `shouldBe` errorText

        it "should parse a closing tag with spaces" do
            let p = parseHsx checkMarkup position extensions "<div></div >"
            p `shouldBe` (Right (Children [Node "div" [] [] False]))

        it "should strip spaces around nodes" do
            let p = parseHsx checkMarkup position extensions "<div> <span> </span> </div>"
            p `shouldBe` (Right (Children [Node "div" [] [Node "span" [] [] False] False]))

        it "should strip spaces after self closing tags" do
            let p = parseHsx checkMarkup position extensions "<head>{\"meta\"}\n\n                        <link rel=\"stylesheet\" href=\"/vendor/bootstrap.min.css\"></head>"
            p `shouldBe` (Right (Children [Node "head" [] [SplicedNode (TH.LitE (TH.StringL "meta")),Node "link" [StaticAttribute "rel" (TextValue "stylesheet"),StaticAttribute "href" (TextValue "/vendor/bootstrap.min.css")] [] True] False]))

        it "should not strip spaces in a text node" do
            let p = parseHsx checkMarkup position extensions " Hello World "
            p `shouldBe` (Right (Children [TextNode "Hello World"]))

        it "should deal with variables in text nodes" do
            let p = parseHsx checkMarkup position extensions "<div>\n    Hello {\"name\"}! \n</div>"
            p `shouldBe`  (Right (Children [Node "div" [] [TextNode "Hello ",SplicedNode (TH.LitE (TH.StringL "name")),TextNode "!"] False]))

        it "should parse self closing tags with spaces around it" do
            let p = parseHsx checkMarkup position extensions " <div/> "
            p `shouldBe`  (Right (Children [Node "div" [] [] False]))

        it "should collapse spaces" do
            let p = parseHsx checkMarkup position extensions "\n    Hello\n    World\n    !    "
            p `shouldBe`  (Right (Children [TextNode "Hello World !"]))
        
        it "should parse spread values" do
            let p = parseHsx checkMarkup position extensions "<div {...variables}/>"
            -- We cannot easily construct the @VarE variables@ expression, therefore we use show here for comparison
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should parse spread values with a space" do
            -- See https://github.com/digitallyinduced/ihp/issues/1588
            let p = parseHsx checkMarkup position extensions "<div { ...variables }/>"
            show p `shouldBe` "Right (Children [Node \"div\" [SpreadAttributes (VarE variables)] [] False])"

        it "should accept underscores in data attributes" do
            let p = parseHsx checkMarkup position extensions "<div data-client_id=\"test\"/>"
            p `shouldBe` (Right (Children [Node "div" [StaticAttribute "data-client_id" (TextValue "test")] [] False]))

        it "should accept doctype" do
            let p = parseHsx checkMarkup position extensions "<!DOCTYPE html><html lang=\"en\"><body>hello</body></html>"
            p `shouldBe` (Right (Children [Node "!DOCTYPE" [StaticAttribute "html" (TextValue "html")] [] True, Node "html" [StaticAttribute "lang" (TextValue "en")] [Node "body" [] [TextNode "hello"] False] False]))


        

module Tests.ParserSpec where

import Test.Hspec
import Compiler.Parser
import Compiler.Types
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "LaTeX Parser" $ do
        it "parses document class" $ do
            let input = T.pack "\\documentclass{article}"
            let expected = Right $ LatexDocument "article" [] []
            parseLatexDoc input `shouldBe` expected

        it "parses simple text" $ do
            let input = T.pack "\\documentclass{article}\\begin{document}Hello, world!\\end{document}"
            let expected = Right $ LatexDocument "article" [] [TextElement "Hello, world!"]
            parseLatexDoc input `shouldBe` expected

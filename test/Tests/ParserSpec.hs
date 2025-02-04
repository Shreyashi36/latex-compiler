module Tests.ParserSpec where

import Test.Hspec
import Test.QuickCheck
import Compiler.Parser
import Compiler.Types
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "LaTeX Parser" $ do
        it "parses document class" $ do
            let input = T.pack "\\documentclass{article}"
            case parseLatexDoc input of
                Right doc -> documentClass doc `shouldBe` "article"
                Left err -> expectationFailure $ show err

        it "parses simple text" $ do
            let input = T.pack "\\documentclass{article}\\begin{document}Hello\\end{document}"
            case parseLatexDoc input of
                Right doc -> body doc `shouldBe` [Text "Hello"]
                Left err -> expectationFailure $ show err

module Tests.ProcessorSpec where

import Test.Hspec
import Compiler.Types
import Compiler.Processor
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "LaTeX Processor" $ do
        it "renders simple document" $ do
            let doc = LatexDocument "article" [] [Text "Hello"]
            result <- processLatex doc
            case result of
                Right text -> T.isInfixOf "Hello" text `shouldBe` True
                Left err -> expectationFailure $ show err

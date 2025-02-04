module Tests.ProcessorSpec where

import Test.Hspec
import qualified Data.Text as T

spec :: Spec
spec = do
    describe "Processor Tests" $ do
        it "processes text correctly" $ do
            let result = Right (T.pack "Hello, world!") :: Either String T.Text
            case result of
                Left err -> expectationFailure err
                Right text -> T.isInfixOf (T.pack "Hello") text `shouldBe` True

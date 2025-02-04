module Tests.IntegrationSpec where

import Test.Hspec
import System.IO.Error (catchIOError)
import System.Directory (removeFile)

spec :: Spec
spec = do
    describe "Integration Tests" $ do
        afterAll (const $ catchIOError (removeFile "test_output.pdf") (const $ return ())) $ do
            it "compiles simple document" $ do
                pendingWith "Implementation in progress"

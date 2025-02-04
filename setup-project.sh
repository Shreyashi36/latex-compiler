#!/bin/bash

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo -e "${BLUE}Setting up LaTeX Compiler project...${NC}"

# Create new project
echo -e "${GREEN}Creating new Stack project...${NC}"
stack new latex-compiler simple
cd latex-compiler

# Create directory structure
echo -e "${GREEN}Creating directory structure...${NC}"
mkdir -p src/Compiler src/Utils test/Tests examples

# Create and populate .gitignore
echo -e "${GREEN}Creating .gitignore...${NC}"
cat > .gitignore << 'EOL'
dist
dist-*
cabal-dev
*.o
*.hi
*.hie
*.chi
*.chs.h
*.dyn_o
*.dyn_hi
.hpc
.hsenv
.cabal-sandbox/
cabal.sandbox.config
*.prof
*.aux
*.hp
*.eventlog
.stack-work/
cabal.project.local
cabal.project.local~
.HTF/
.ghc.environment.*
*.pdf
EOL

# Create and populate LICENSE
echo -e "${GREEN}Creating LICENSE file...${NC}"
cat > LICENSE << 'EOL'
MIT License

Copyright (c) 2024

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
EOL

# Create and populate .cabal file
echo -e "${GREEN}Creating .cabal file...${NC}"
cat > latex-compiler.cabal << 'EOL'
name:                latex-compiler
version:             0.1.0.0
synopsis:            A LaTeX compiler written in Haskell
description:         A tool to compile LaTeX documents to PDF using Haskell
license:             MIT
license-file:        LICENSE
author:              Your Name
maintainer:          your.email@example.com
category:            Text
build-type:          Simple
cabal-version:       >=1.10

executable latex-compiler
  main-is:             Main.hs
  other-modules:       Compiler.Parser
                     , Compiler.Types
                     , Compiler.Processor
                     , Compiler.PDF
                     , Utils.FileHandler
                     , Utils.Error
  hs-source-dirs:      src
  build-depends:       base >= 4.7 && < 5
                     , parsec >= 3.1
                     , text >= 1.2
                     , containers >= 0.6
                     , process >= 1.6
                     , directory >= 1.3
                     , filepath >= 1.4
                     , mtl >= 2.2
  default-language:    Haskell2010
  ghc-options:         -Wall -Wcompat

test-suite latex-compiler-test
  type:                exitcode-stdio-1.0
  main-is:             Spec.hs
  other-modules:       Tests.ParserSpec
                     , Tests.ProcessorSpec
                     , Tests.IntegrationSpec
  hs-source-dirs:      test
  build-depends:       base
                     , latex-compiler
                     , hspec >= 2.7
                     , QuickCheck >= 2.14
  default-language:    Haskell2010
  ghc-options:         -Wall -Wcompat
EOL

# Create and populate source files
echo -e "${GREEN}Creating source files...${NC}"

# Main.hs
cat > src/Main.hs << 'EOL'
module Main where

import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import Compiler.Parser (parseLatexDoc)
import Compiler.Processor (processLatex)
import Compiler.PDF (generatePDF)
import Utils.FileHandler (readLatexFile, writeOutputFile)
import Utils.Error (CompilerError(..), handleError)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [input, output] -> do
            result <- processFile input output
            case result of
                Left err -> handleError err
                Right _ -> putStrLn "Successfully compiled LaTeX document"
        _ -> putStrLn "Usage: latex-compiler input.tex output.pdf"

processFile :: FilePath -> FilePath -> IO (Either CompilerError ())
processFile input output = do
    content <- readLatexFile input
    case parseLatexDoc content of
        Left err -> return $ Left $ ParseError $ show err
        Right doc -> do
            processed <- processLatex doc
            case processed of
                Left err -> return $ Left err
                Right processedDoc -> do
                    writeOutputFile (output ++ ".tex") processedDoc
                    generatePDF output
EOL

# Types.hs
cat > src/Compiler/Types.hs << 'EOL'
module Compiler.Types where

import Data.Text (Text)

data LatexElement
    = Environment String [LatexElement]
    | Command String [String]
    | Text String
    | Math String
    deriving (Show, Eq)

data LatexDocument = LatexDocument
    { documentClass :: String
    , preamble :: [LatexElement]
    , body :: [LatexElement]
    } deriving (Show, Eq)
EOL

# Parser.hs
cat > src/Compiler/Parser.hs << 'EOL'
module Compiler.Parser where

import Text.Parsec
import Text.Parsec.Text
import qualified Data.Text as T
import Control.Monad (void)
import Compiler.Types

parseLatexDoc :: T.Text -> Either ParseError LatexDocument
parseLatexDoc = parse documentParser ""

documentParser :: Parser LatexDocument
documentParser = do
    cls <- documentClassParser
    pre <- preambleParser
    bod <- bodyParser
    return $ LatexDocument cls pre bod

documentClassParser :: Parser String
documentClassParser = do
    string "\\documentclass{"
    cls <- many1 letter
    char '}'
    return cls

preambleParser :: Parser [LatexElement]
preambleParser = many $ choice
    [ try commandParser
    , try environmentParser
    ]

bodyParser :: Parser [LatexElement]
bodyParser = between
    (string "\\begin{document}")
    (string "\\end{document}")
    (many elementParser)

elementParser :: Parser LatexElement
elementParser = choice
    [ try commandParser
    , try environmentParser
    , try mathParser
    , textParser
    ]

commandParser :: Parser LatexElement
commandParser = do
    char '\\'
    name <- many1 letter
    args <- many argumentParser
    return $ Command name args

environmentParser :: Parser LatexElement
environmentParser = do
    string "\\begin{"
    name <- many1 letter
    char '}'
    content <- manyTill elementParser (try $ string "\\end{" >> string name >> char '}')
    return $ Environment name content

argumentParser :: Parser String
argumentParser = between (char '{') (char '}') (many $ noneOf "}")

mathParser :: Parser LatexElement
mathParser = between (char '$') (char '$') $
    Math <$> many (noneOf "$")

textParser :: Parser LatexElement
textParser = Text <$> many1 (noneOf "\\${}") 
EOL

# Processor.hs
cat > src/Compiler/Processor.hs << 'EOL'
module Compiler.Processor where

import Compiler.Types
import Utils.Error
import qualified Data.Text as T

processLatex :: LatexDocument -> IO (Either CompilerError T.Text)
processLatex doc = do
    -- Process the document
    return $ Right $ renderDocument doc

renderDocument :: LatexDocument -> T.Text
renderDocument doc = T.unlines
    [ renderDocumentClass (documentClass doc)
    , renderPreamble (preamble doc)
    , "\\begin{document}"
    , renderBody (body doc)
    , "\\end{document}"
    ]

renderDocumentClass :: String -> T.Text
renderDocumentClass cls = T.pack $ "\\documentclass{" ++ cls ++ "}"

renderPreamble :: [LatexElement] -> T.Text
renderPreamble elements = T.unlines $ map renderElement elements

renderBody :: [LatexElement] -> T.Text
renderBody elements = T.unlines $ map renderElement elements

renderElement :: LatexElement -> T.Text
renderElement (Text t) = T.pack t
renderElement (Math m) = T.pack $ "$" ++ m ++ "$"
renderElement (Command name args) = 
    T.pack $ "\\" ++ name ++ concatMap (\arg -> "{" ++ arg ++ "}") args
renderElement (Environment name content) =
    T.unlines
        [ T.pack $ "\\begin{" ++ name ++ "}"
        , T.unlines $ map renderElement content
        , T.pack $ "\\end{" ++ name ++ "}"
        ]
EOL

# PDF.hs
cat > src/Compiler/PDF.hs << 'EOL'
module Compiler.PDF where

import System.Process (callCommand)
import System.Directory (removeFile)
import System.IO.Error (catchIOError)
import Utils.Error

generatePDF :: FilePath -> IO (Either CompilerError ())
generatePDF output = do
    result <- runPdfLatex output
    cleanupAuxFiles output
    return result

runPdfLatex :: FilePath -> IO (Either CompilerError ())
runPdfLatex output = do
    let command = "pdflatex -interaction=nonstopmode " ++ output ++ ".tex"
    catchIOError
        (Right <$> callCommand command)
        (return . Left . PDFGenerationError . show)

cleanupAuxFiles :: FilePath -> IO ()
cleanupAuxFiles output = do
    let extensions = [".aux", ".log"]
    mapM_ (\ext -> catchIOError (removeFile $ output ++ ext) (const $ return ())) extensions
EOL

# Error.hs
cat > src/Utils/Error.hs << 'EOL'
module Utils.Error where

data CompilerError
    = ParseError String
    | ProcessingError String
    | PDFGenerationError String
    deriving (Show, Eq)

handleError :: CompilerError -> IO ()
handleError (ParseError msg) = putStrLn $ "Parse error: " ++ msg
handleError (ProcessingError msg) = putStrLn $ "Processing error: " ++ msg
handleError (PDFGenerationError msg) = putStrLn $ "PDF generation error: " ++ msg
EOL

# FileHandler.hs
cat > src/Utils/FileHandler.hs << 'EOL'
module Utils.FileHandler where

import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, SomeException)
import Utils.Error

readLatexFile :: FilePath -> IO (Either CompilerError T.Text)
readLatexFile path = catch
    (Right <$> TIO.readFile path)
    (\(e :: SomeException) -> return $ Left $ ProcessingError $ show e)

writeOutputFile :: FilePath -> T.Text -> IO (Either CompilerError ())
writeOutputFile path content = catch
    (Right <$> TIO.writeFile path content)
    (\(e :: SomeException) -> return $ Left $ ProcessingError $ show e)
EOL

# Create and populate test files
echo -e "${GREEN}Creating test files...${NC}"

# Spec.hs
cat > test/Spec.hs << 'EOL'
{-# OPTIONS_GHC -F -pgmF hspec-discover #-}
EOL

# ParserSpec.hs
cat > test/Tests/ParserSpec.hs << 'EOL'
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
EOL

# ProcessorSpec.hs
cat > test/Tests/ProcessorSpec.hs << 'EOL'
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
EOL

# IntegrationSpec.hs
cat > test/Tests/IntegrationSpec.hs << 'EOL'
module Tests.IntegrationSpec where

import Test.Hspec
import System.Directory (removeFile)
import System.IO.Error (catchIOError)

spec :: Spec
spec = do
    describe "Integration Tests" $ do
        it "compiles simple document" $ do
            -- Test end-to-end compilation
            pendingWith "Implementation in progress"

        afterAll $ do
            -- Clean up generated files
            catchIOError (removeFile "test_output.pdf") (const $ return ())
EOL

# Create example LaTeX files
echo -e "${GREEN}Creating example files...${NC}"

# simple.tex
cat > examples/simple.tex << 'EOL'
\documentclass{article}
\begin{document}
Hello, World!
\end{document}
EOL

# math.tex
cat > examples/math.tex << 'EOL'
\documentclass{article}
\begin{document}
The quadratic formula is $x = \frac{-b \pm \sqrt{b^2 - 4ac}}{2a}$
\end{document}
EOL

# complex.tex
cat > examples/complex.tex << 'EOL'
\documentclass{article}
\begin{document}
\section{Introduction}
This is a more complex document with sections.

\subsection{Math}
Here's some math: $E = mc^2$

\begin{itemize}
\item First item
\item Second item
\end{itemize}
\end{document}
EOL

# Initialize git repository
echo -e "${GREEN}Initializing git repository...${NC}"
git init
git add .
git commit -m "Initial commit"

# Build the project
echo -e "${GREEN}Building the project...${NC}"
stack build

echo -e "${BLUE}Project setup complete!${NC}"
echo -e "${GREEN}To run the
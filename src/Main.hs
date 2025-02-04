module Main where

import qualified Data.Text.IO as TIO
import Compiler.Types
import Compiler.Parser
import Compiler.Processor (processLatex)
import Utils.FileHandler (readLatexFile, writeOutputFile)
import System.Environment (getArgs)
import Compiler.PDF (generatePDF)
import Utils.Error (CompilerError(..), handleError)

main :: IO ()
main = do
    content <- readLatexFile "input.tex"
    case content of
        Left err -> putStrLn $ "Error reading file: " ++ show err
        Right textContent -> 
            case parseLatexDoc textContent of
                Left err -> putStrLn $ "Parse error: " ++ show err
                Right doc -> do
                    processed <- processLatex doc
                    case processed of
                        Left err -> putStrLn $ "Processing error: " ++ show err
                        Right processedDoc -> putStrLn $ "Processed document: " ++ show processedDoc

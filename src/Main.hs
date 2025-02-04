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

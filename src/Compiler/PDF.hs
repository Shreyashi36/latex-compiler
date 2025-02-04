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

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

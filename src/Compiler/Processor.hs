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

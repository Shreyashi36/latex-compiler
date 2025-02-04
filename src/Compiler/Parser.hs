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

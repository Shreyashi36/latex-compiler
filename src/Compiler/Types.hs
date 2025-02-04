module Compiler.Types where

import Data.Text ()

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

module Lexer where

import Data.Char
import Exprs

getWord :: String -> String
getWord [] = []
getWord (x:xs) = case elem x ".,?" of
                   True -> ' ':x:getWord xs
                   False -> x:getWord xs

-- | Split a string by its whitespace.
split :: String -> [String]
split "" = []
split (' ':xs) = "":split xs
split [x] = [[x]]
split (x:xs) = let (y:ys) = split xs
               in ((x:y):ys)

-- | Stop parsing after a sentence boundary.
preproc :: [String] -> [String]
preproc [] = []
preproc (",":xs) = preproc xs
preproc (".":xs) = []
preproc ("?":xs) = []
preproc (x:xs) = x:preproc xs

lexer :: String -> [String]
lexer = filter (/= "") . preproc . split . getWord . map toLower

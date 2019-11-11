module Main where

import MG_parser
import Exprs
import Read_lexicon

main :: IO ()
main = do putStrLn "Specify the file containing your lexicon:"
          filename <- getLine
          contents <- readFile filename
          let lex = map read (lines contents)
          putStrLn "String to parse:"
          str <- getLine
          putStrLn "Feature sequence:"
          seq <- getLine
          putStrLn $ show $ firstParse lex (unFeats $ read seq) str

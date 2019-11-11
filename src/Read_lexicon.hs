module Read_lexicon where

import Exprs

-- | Get the phonological component (string) of a lexical entry.
getPhon :: String -> String
getPhon = reverse . dropWhile (== ' ') . reverse . takeWhile (/= ':')

-- | Get the feature sequence (string) of a lexical entry.
getFeats :: String -> String
getFeats = tail . tail . dropWhile (/= ':') . filter (/= ' ')

-- | List the individual features (strings) of a sequence.
listFeats :: String -> [String]
listFeats "" = []
listFeats ('.':xs) = listFeats xs
listFeats p = takeWhile (/= '.') p : listFeats (dropWhile (/= '.') p)

-- | Parse a string into a feature.
parseFeat :: String -> Feat
parseFeat [c] = Feat c (Mrg RPlus)
parseFeat ['=', c] = Feat c (Mrg RMinus0)
parseFeat [c, '='] = Feat c (Mrg RMinus1)
parseFeat ['+', c] = Feat c (Mv VPlus)
parseFeat ['-', c] = Feat c (Mv VMinus)

-- | Read a string as an 'Exprs.Expr'.
instance Read Expr where
  readsPrec _ input = [(Expr
                          (getPhon input)
                          (map parseFeat $ listFeats $ getFeats input)
                       , "")]

newtype Feats = Feats { unFeats :: [Feat] } deriving (Eq, Show)

-- | Read a string as a list of 'Exprs.Feat'.
instance Read Feats where
  readsPrec _ input = [(Feats $
                          map parseFeat $
                            listFeats $
                              filter (/= ' ') input
                       , "")]

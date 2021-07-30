module Exprs where

import Control.Monad
import Data.List

-- $ intro
-- This module defines MG expressions as the datatype 'Expr'.

-- | An operation is either move ('R') or merge ('V').
data Op = R | V deriving Eq

-- | Polarities for merge features.
data RPol = RPlus | RMinus0 | RMinus1 deriving Eq

-- | Polarities for move features.
data VPol = VPlus | VMinus deriving Eq

-- | Polarities, in general.
data Pol = Mrg RPol | Mv VPol deriving Eq

-- | Features.
data Feat = Feat Char Pol deriving Eq

-- | Show according to the mg notation for features.
instance Show Feat where
  show (Feat c (Mrg RPlus)) = [c]
  show (Feat c (Mrg RMinus0)) = "=" ++ [c]
  show (Feat c (Mrg RMinus1)) = c:"="
  show (Feat c (Mv VPlus)) = "+" ++ [c]
  show (Feat c (Mv VMinus)) = "-" ++ [c]

-- | Get the category of a feature.
cat :: Feat -> Char
cat (Feat c _) = c

-- | Get the operation associated with a feature.
op :: Feat -> Op
op (Feat _ (Mrg _)) = R
op (Feat _ (Mv _)) = V

-- | Get the polarity of a feature.
pol :: Feat -> Pol
pol (Feat _ p) = p 

-- | Reverse the polarity of a feature.
reversePol :: Feat -> Feat
reversePol (Feat c (Mrg RPlus)) = Feat c (Mrg RMinus0)
reversePol (Feat c (Mrg _)) = Feat c (Mrg RPlus)
reversePol (Feat c (Mv VPlus)) = Feat c (Mv VMinus)
reversePol (Feat c (Mv VMinus)) = Feat c (Mv VPlus)

-- | Expressions have a phonological component and a list of features.
data Expr = Expr { phon :: String, feats :: [Feat] } deriving Eq

instance Show Expr where
  show e = phon e ++ " :: " ++ drop 1 (join $ map (('.':) . show) (feats e))

-- | An expression's first feature.
firstFeat :: Expr -> Feat
firstFeat = head . feats

-- | A type for lexica.
type Lexicon = [Expr]

-- | Categories from the lexicon.
getCats :: Lexicon -> [Char]
getCats = join . map (map cat . feats)

-- | Features from the lexicon.
possibleFeats :: Lexicon -> [Feat]
possibleFeats lex = [ f | e <- lex, f <- feats e ]

-- | Get a list of the suffixes of a list.
getSuffixes :: [a] -> [[a]]
getSuffixes [] = []
getSuffixes (x:xs) = (x:xs) : getSuffixes xs

-- | Get a list of the suffixes of feature sequences from the lexicon.
featSuffixes :: Lexicon -> [[Feat]]
featSuffixes lex = [ suf | seq <- map feats lex, suf <- getSuffixes seq ]

-- | Lists of expressions are manipluatd by mg derivations.
data Exprs = Exprs { unExprs :: [Expr] } deriving Eq

-- | 'Exprs' can be shown.
instance Show Exprs where
  show = join . map ((++ " ; ") . show) . unExprs

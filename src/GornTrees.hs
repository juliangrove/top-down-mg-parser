module GornTrees where

import Control.Monad
import Data.List
import Exprs

-- $ intro
-- This module defines the data structure encoding parser states (i.e., stacks)
-- as lists of 'GornTree's, which, in turn, are lists of 'GornNode's, i.e.,
-- pairs of 'Gorn' addresses and feature sequences.

-- | Gorn addresses.
newtype Gorn = Gorn { unGorn :: [Int] } deriving Eq

-- | Show Gorn addresses as 'Int' lists.
instance Show Gorn where
  show = show . unGorn

-- | The nodes of a tree are given by Gorn addresses coinciding with feature
-- sequences.
data GornNode = GornNode { gorn :: Gorn, gnfeats :: [Feat] } deriving (Eq, Show)

-- | The first feature of a 'GornNode'.
gnfeat :: GornNode -> Feat
gnfeat = head . gnfeats

-- | A tree is a list of its nodes.
type GornTree = [GornNode]

-- | The states of our parser.
type Stack = [GornTree]

-- | Two 'Gorn' addresses are ordered by precedence in the 'GornTree'.
prec :: Gorn -> Gorn -> Bool
prec (Gorn []) (Gorn []) = True
prec (Gorn (x:xs)) (Gorn (y:ys)) = x < y || (x == y && prec (Gorn xs) (Gorn ys))
prec _ _ = False

instance Ord Gorn where
  (<=) = prec

-- | Sort a 'Stack' by the most precedent 'Gorn' address of its element trees.
sortByGorn :: Stack -> Stack
sortByGorn = sortOn $ head . sort . map gorn

module MG_parser where

import Control.Monad.State
import Data.List
import Exprs
import GornTrees
import Lexer

-- $ intro
-- This module implements the basic parsing operations 'unmerge1c', 'unmerge2c',
-- 'unmerge1s', 'unmerge2s', and 'scan', which are defined in Kobele, Gerth, and
-- Hale 2013. The resulting parser is 'StateT Stack []' monadic with values
-- consisting of lists of pairs of expressions from the lexicon and their Gorn
-- address in the derived tree. The state is a stack of intermediate parse
-- trees, and the ``inner" list monad encodes the non-determinism of the
-- transitions from one stack state to the next.

-- | scan (Kobele et al. 2013, section 4.1, p. 39)
scan :: Lexicon -> StateT Stack [] [(Expr, Gorn)]
scan lex = StateT $ \stk ->
             do guard $ length stk > 0
                guard $ length (head stk) == 1
                let gn0 = head (head stk)
                e <- lex
                guard $ gnfeats gn0 == feats e
                pure ([(e, gorn gn0)], tail stk)

-- | unmerge1c (Kobele et al. 2013, section 4.1, p. 40)
unmerge1c :: Lexicon -> GornTree -> [Stack]
unmerge1c lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g) fs = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       newf:newfs <- [ Feat c (Mrg RMinus0) : fs | c <- getCats lex,
                       (Feat c (Mrg RMinus0) : fs) `elem` (featSuffixes lex),
                       [Feat c (Mrg RPlus)] `elem` (featSuffixes lex) ]
       pure [GornNode (Gorn $ g ++ [0]) (newf:newfs) : lft,
             GornNode (Gorn $ g ++ [1]) [reversePol newf] : rgt]

-- | unmerge2c (Kobele et al. 2013, section 4.1, p. 40)
unmerge2c :: Lexicon -> GornTree -> [Stack]
unmerge2c lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g0) fs0 = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       guard $ length rgt > 0
       let GornNode g1 fs1 : gns1 = rgt
       newf:newfs <- [ Feat c (Mrg RMinus0) : fs0 | c <- getCats lex,
                       (Feat c (Mrg RMinus0) : fs0) `elem` (featSuffixes lex),
                       (Feat c (Mrg RPlus) : fs1) `elem` (featSuffixes lex) ]
       pure [GornNode g1 (reversePol newf : fs1) : gns1,
             GornNode (Gorn $ g0 ++ [0]) (newf:newfs) : lft]

-- | unmerge1s (Kobele et al. 2013, section 4.1, p. 39)
unmerge1s :: Lexicon -> GornTree -> [Stack]
unmerge1s lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g) fs = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       newf:newfs <- [ Feat c (Mrg RMinus1) : fs | c <- getCats lex,
                       (Feat c (Mrg RMinus1) : fs) `elem` (featSuffixes lex),
                       [Feat c (Mrg RPlus)] `elem` (featSuffixes lex) ]
       pure [GornNode (Gorn $ g ++ [0]) [reversePol newf] : rgt,
             GornNode (Gorn $ g ++ [1]) (newf:newfs) : lft]

-- | unmerge2s (Kobele et al. 2013, section 4.1, p. 39)
unmerge2s :: Lexicon -> GornTree -> [Stack]
unmerge2s lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g0) fs0 = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       guard $ length rgt > 0
       let GornNode g1 fs1 : gns1 = rgt
       newf:newfs <- [ Feat c (Mrg RMinus1) : fs0 | c <- getCats lex,
                       (Feat c (Mrg RMinus1) : fs0) `elem` (featSuffixes lex),
                       (Feat c (Mrg RPlus) : fs1) `elem` (featSuffixes lex) ]
       pure [GornNode g1 (reversePol newf : fs1) : gns1,
             GornNode (Gorn $ g0 ++ [1]) (newf:newfs) : lft]

-- | unmove1 (Kobele et al. 2013, section 4.1, p. 40)
unmove1 :: Lexicon -> GornTree -> [Stack]
unmove1 lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g) fs = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       newf:newfs <- [ Feat c (Mv VPlus) : fs | c <- getCats lex,
                       (Feat c (Mv VPlus) : fs) `elem` (featSuffixes lex),
                       [Feat c (Mv VMinus)] `elem` (featSuffixes lex) ]
       guard $ reversePol newf `notElem` map gnfeat gns
       pure [GornNode (Gorn $ g ++ [1]) (newf:newfs) : lft ++
             GornNode (Gorn $ g ++ [0]) [reversePol newf] : rgt]

-- | unmove2 (Kobele et al. 2013, section 4.1, p. 40)
unmove2 :: Lexicon -> GornTree -> [Stack]
unmove2 lex gt
  = do guard $ length gt > 0
       let GornNode (Gorn g0) fs0 = head gt
           gns = tail gt
       (lft, rgt) <- [ splitAt n gns | n <- [0..length gns] ]
       guard $ length rgt > 0
       let GornNode g1 fs1 : gns1 = rgt
       newf:newfs <- [ Feat c (Mv VPlus) : fs0 | c <- getCats lex,
                       (Feat c (Mv VPlus) : fs0) `elem` (featSuffixes lex),
                       (Feat c (Mv VMinus) : fs1) `elem` (featSuffixes lex) ]
       guard $ reversePol newf `notElem` map gnfeat gns
       pure [GornNode (Gorn $ g0 ++ [1]) (newf:newfs) : lft ++
             GornNode g1 (reversePol newf : fs1) : gns1]


-- | Combine two functions onto lists by concatenating the results.
(<//>) :: (a -> [b]) -> (a -> [b]) -> a -> [b]
f <//> g = join $ (. g) . (++) . f

-- | Applicative concatentation for list values.
(<++>) :: Applicative m => m [a] -> m [a] -> m [a]
m <++> n = pure (++) <*> m <*> n

-- | Concatenate the lists of objects in the 'StateT s []' monad.
(</>) :: StateT s [] a -> StateT s [] a -> StateT s [] a
m </> n = StateT $ \s -> runStateT m s ++ runStateT n s

-- | Apply a non-deterministic function (a -> [[a]]) to the head of an [a] by
-- non-deterministically contatenating the result to the tail.
newStacks :: (a -> [[a]]) -> [a] -> [[a]]
newStacks f stk = [ stk' | length stk > 0,
                    stk' <- (f (head stk)) <++> pure (tail stk) ]

-- | Non-deterministically take a stack onto its next state via any of the
-- applicable unmerge or unmove operations. Sort the resulting stack by linear
-- precedence of the Gorn addresses of the nodes of its element trees (Kobele et
-- al. 2013, section 4.1, p. 39).
unDerive :: Lexicon -> Stack -> [Stack]
unDerive lex = map sortByGorn . newStacks (unmerge1c lex <//>
                                           unmerge2c lex <//>
                                           unmerge1s lex <//>
                                           unmerge2s lex <//>
                                           unmove1 lex   <//>
                                           unmove2 lex)

-- | Take the state of the stack onto its non-deterministic next state by
-- performing either an unmerge step or an unmove step.
unDeriveStep :: Lexicon -> StateT Stack [] [(Expr, Gorn)]
unDeriveStep lex = StateT $ \s -> [ ([], s') | s' <- unDerive lex s ]

-- | Get a list of the prefixes of a list.
getPrefixes :: [a] -> [[a]]
getPrefixes [] = []
getPrefixes (x:xs) = [x] : map (x:) (getPrefixes xs)

-- | Given a list of strings @strs@, @matchToPrefix strs@ is a Kleisli arrow
-- checking whether its argument ``matches" a prefix of @strs@. 
matchToPrefix :: [String] -> [(Expr, Gorn)] -> StateT Stack [] [(Expr, Gorn)]
matchToPrefix strs output = do prefix <- lift $ [] : getPrefixes strs
                               guard $ (output >>= phon . fst)
                                       == join prefix
                               pure output

-- | Remove duplicate elements of a stateful list.
nubState :: (Eq a, Eq s) => StateT s [] a -> StateT s [] a
nubState m = StateT $ \s -> nub $ runStateT m s

-- | Sort a stateful list by some measure.
sortState :: Ord b => ((a, s) -> b) -> StateT s [] a -> StateT s [] a
sortState f m = StateT $ \s -> sortOn f $ runStateT m s

-- | Keep the values returned once the stack state becomes empty.
keepDone :: StateT Stack [] [(Expr, Gorn)]
keepDone = StateT $ \s -> [ ([], s) | s == [] ]

-- | Non-deterministically scan and underive the current state, retaining prior
-- values; remove duplicates; match the values produced to a prefix of the
-- string being parsed; sort the values by length (longest first).
fullCycle :: Lexicon
          -> [String]
          -> StateT Stack [] [(Expr, Gorn)]
          -> StateT Stack [] [(Expr, Gorn)]
fullCycle lex strs m = sortState ((* (-1)) . length . map fst . fst) $
                         (nubState $ m <++> (unDeriveStep lex </>
                                             scan lex         </>
                                             keepDone))
                         >>= matchToPrefix strs

-- | Run cycles until a parse is found matching the input string.
cycleUntilParsed :: Lexicon
                 -> [String]
                 -> StateT Stack [] [(Expr, Gorn)]
                 -> StateT Stack [] [(Expr, Gorn)]
cycleUntilParsed lex strs m
  = if (filter ((==[]) . snd)
         (filter ((== join strs) . (\(v, s) -> v >>= phon . fst))
           (runStateT m [])))
       == []
      then cycleUntilParsed lex strs (fullCycle lex strs m)
      else m

-- | Retrieve the first parse found of the input string.
firstParse :: Lexicon -> [Feat] -> String -> [(Expr, Gorn)]
firstParse lex feats str
  = fst . head $ flip runStateT [] $ cycleUntilParsed
                                       lex
                                       (lexer str)
                                       (put [[GornNode (Gorn []) feats]]
                                        >> pure [])

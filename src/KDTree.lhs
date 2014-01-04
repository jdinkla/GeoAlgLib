%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{$k$-d-Bäume (|KDTree|)}
\module{KDTree}

\begin{code}
module KDTree (
         KDTree, empty, fromList, member, rangeQuery, toList, pprint, module Assocs
       ) where

import Point     ( Point (..), inInterval, compareIth )
import qualified Point (dimension,toList )
import Divide    ( divideBy )
import Assocs
import Basics.Utilities ( relToFst )
import Basics.ListCat	 ( ListCat, nil, unit, cat, list )
import qualified Basics.Pretty as Pr
import Basics.Sorting   ( nubSortedBy )
\end{code}

\begin{code}
data KDTree a		      = Nil 
			      | Leaf a
			      | Node Int a (KDTree a) (KDTree a) (KDTree a) 

empty                         :: KDTree a
empty                         = Nil

depth			      :: KDTree a -> Int
depth Nil		      = 0
depth (Leaf _)		      = 1
depth (Node _ _ ls es rs)     = 1 + maximum [depth ls, depth es, depth rs]


fromList                      :: (Point p, Ord a, Num a) => [p a] -> KDTree (p a)
fromList []                   = Nil
fromList ps                   = build 1 ps 
  where 
  k                           = dimension (head ps)
  build _ []                  = Nil
  build _ [x]                 = Leaf x
  build i xs                  = Node i m (build j ls) 
					 (build j es)
					 (build j hs)
    where (m, (ls, es, hs))   = divideBy (compareIth i) xs
          j                   = (i `mod` k) + 1

toList                        :: KDTree a -> [a]
toList                        = list . elems
  where
  elems Nil                   = nil
  elems (Leaf p)              = unit p
  elems (Node _ _ ls es hs )  = elems ls `cat` (elems es `cat` elems hs)


member                        :: (Point p, Ord a, Num a) => p a -> KDTree (p a) -> Bool
member _ Nil                  = False
member q (Leaf p)             = p <==> q
member q (Node i m ls es hs)  = case compareIth i q m of
                                  LT -> q `member` ls
                                  EQ -> q `member` es
                                  GT -> q `member` hs


rangeQuery                    :: (Point p, Ord a, Num a) 
			      => KDTree (p a) -> (p a, p a) -> [p a]
rangeQuery kd (p,q)           = list (query kd)
  where
  query Nil     	      = nil

  query (Leaf r)              
    | r `inInterval` (p,q)    = unit r 
    | otherwise		      = nil

  query (Node i m ls es rs)   = chk (compareIth i m p) (compareIth i m q)
    where
    chk LT _		      = query rs		-- m<ith i p             
    chk EQ _		      = query es `cat` query rs -- m == ith i p
    chk GT GT		      = query ls		-- m > ith i q
    chk GT EQ		      = query ls `cat` query es -- m == ith i q	      
    chk GT LT                 = query ls `cat` (query es `cat` query rs)


pprint                        :: Show a => KDTree a -> String
pprint			      = Pr.render . pprintD

pprintD                       :: Show a => KDTree a -> Pr.Doc
pprintD Nil                   = Pr.text "Nil"
pprintD (Leaf v)	      = Pr.text ("Leaf " ++ show v)
pprintD (Node j a ls es hs)   = Pr.text ("Node ") 
                                  Pr.<> ((Pr.text (show j ++ " " ++ show a ++ " "))
                                         Pr.$$ (Pr.nest 0 (pprintD ls Pr.$$ pprintD es 
                                         Pr.$$ pprintD hs)))
\end{code}

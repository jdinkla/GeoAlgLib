%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Bereichsbäume (|RangeTree|)}
\module{RangeTree}

\begin{code}
module RangeTree (
         RangeTree, empty, fromList, toList, rangeQuery, member, pprint
       ) where

import Point     ( Point (..), compareIth, inInterval, inIntervalIth )
import Divide    ( divideBy )
import Basics.Utilities ( minimumWith, maximumWith )
import Basics.ListCat   
import qualified Basics.Pretty as Pr


data RangeTree a 	      = Nil 
			      | Leaf a 
			      | MultiLeaf [a] (RangeTree a)
			      | Node a (a, a) (RangeTree a) (RangeTree a) (RangeTree a)

-- Suchbauminvariante: (Node m _ ls hs _)
-- =>
--	all (\ x -> ith i x < ith i m) ls
-- 	all (\ x -> ith i x >= ith i m) hs

empty			      :: RangeTree a
empty 			      = Nil

nodes			      :: RangeTree a -> Int
nodes Nil		      = 0
nodes (Leaf _)		      = 1
nodes (MultiLeaf _ rt)	      = 1 + nodes rt 
nodes (Node _ _ ls hs rs)     = 1 + nodes ls + nodes hs + nodes rs


fromList                      :: (Point p, Ord a, Num a) => [p a] -> RangeTree (p a)
fromList []		      = Nil
fromList xs		      = build 1 xs
  where 
  k		              = dimension (head xs)
  build i []		      = Nil
  build i [x]		      = Leaf x
  build i xs		      = if i>k then Nil else node i xs

  node i xs
    | null ls && null hs      = MultiLeaf es tree
    | null ls   	      = Node min_hs int	(build i es) (build i hs) tree
    | otherwise 	      = Node m int
				       (build i ls) 
				       (build i (es++hs)) 
				       tree

    where 
    (m, (ls, es, hs))         = divideBy (compareIth i) xs
    tree		      = build (i+1) xs
    min_hs		      = minimumWith (ith i) hs
    int			      = (minimumWith (ith i) xs, maximumWith (ith i) xs)


toList			      :: RangeTree a -> [a]
toList			      = list . elems

elems			      :: RangeTree a -> ListCat a
elems Nil		      = nil
elems (Leaf x)	              = unit x
elems (MultiLeaf xs _)        = toListCat xs
elems (Node _ _ ls hs _)      = elems ls `cat` elems hs


member			      :: (Point p, Ord a, Num a) => p a -> RangeTree (p a) -> Bool
member q rtree		      = search 1 q rtree
  where
  search i q Nil	      = False
  search i q (Leaf x)	      = x <==> q
  search i q (MultiLeaf _ rtree)  = search (i+1) q rtree
  search i q (Node m _ ls rs _)    
    | ith i q < ith i m	      = search i q ls
    | otherwise		      = search i q rs


rangeQuery		      :: (Point p, Ord a, Num a) => RangeTree (p a) -> (p a, p a) -> [p a]
rangeQuery rt (p,q)	      = list (query 1 rt)
  where
  k			      = dimension p

  query i Nil		      = nil

  query i (Leaf x)
    | x `inInterval` (p,q)    = unit x
    | otherwise		      = nil

  query i (MultiLeaf xs rt)
    | inInterval && i==k      = toListCat xs
    | inInterval	      = query (i+1) rt
    | otherwise		      = nil
    where inInterval	      = inIntervalIth i (head xs) (p,q)

  query i node@(Node m (l,r) ls rs rtree)
    | r_i < p_i || l_i > q_i  = nil
    | l_i >= p_i && r_i <= q_i= if i==k then elems node
				else query (i+1) rtree
    | otherwise		      = query i ls `cat` query i rs
    where r_i		      = ith i r
	  l_i		      = ith i l
	  q_i		      = ith i q
	  p_i		      = ith i p


pprint			      :: Show a => RangeTree a -> String
pprint			      = Pr.render . ppr 1

ppr			      :: Show a => Int -> RangeTree a -> Pr.Doc
ppr i Nil		      = Pr.text "Nil"
ppr i (Leaf x)		      = Pr.text ("Leaf (" ++ show i ++") ") Pr.<> (Pr.text (show x))
ppr i (MultiLeaf xs rs)	      = Pr.text ("MultiLeaf (" ++ show i ++") ") Pr.<> (Pr.text (show xs) Pr.$$ ppr (i+1) rs)

ppr i (Node m (l,r) ls rs ys) = Pr.text ("Node ("++show i++") ") Pr.<> (Pr.text (show (m, l, r))
					Pr.$$ ppr (i+1) ys Pr.$$ ppr i ls Pr.$$ ppr i rs)
\end{code}


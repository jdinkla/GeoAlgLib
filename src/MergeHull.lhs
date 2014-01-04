%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{MergeHull (|Mergehull|)}
\module{MergeHull}

\begin{code}
module MergeHull ( 
         mergeHull, incrementalMergeHull, bottomupMergeHull, 
	 insert, merge, lowerBridge, upperBridge
       )
where

import Point2    ( P2, Point2, equalX, leftOrBelow,
		   below, lexic2, compareX,
		   leftest, rightest, 
		   isLeftTurn, isRightTurn, clockwise3 )
import Polygon   ( Polygon (PolygonCW), Polygon2, inConvex, deleteCollinear )
import List      ( groupBy )
import Divide    ( SplitTree (..), splitTree, reduce )
import Basics.Utilities ( rotateTo, extremaBy, Rel3, sublist )
import Basics.Sorting   ( sortBy )
\end{code}

siehe \cite[K. 5.1]{chen96:cg}, \cite[K. 3.8]{orourke94:cg}

Als Abkürzungen definieren wir
\begin{code}
filterAndSort	              :: (Ord a, Num a) => [P2 a] -> [[P2 a]]
filterAndSort		      = map (extremaBy below) . groupBy equalX 
			      . sortBy leftOrBelow
\end{code}

\begin{code}
mergeHull                     :: (Ord a, Num a) => [P2 a] -> Polygon2 a
mergeHull                     = PolygonCW . deleteCollinear
			      . reduce (extremaBy below) merge
			      . splitTree lexic2 compareX

merge                         :: (Ord a, Num a) => [P2 a] -> [P2 a] -> [P2 a]
merge ls rs                   = sublist ur lr rs ++ sublist ll ul ls
  where (ul, ur)	      = upperBridge ls rs
        (ll, lr)	      = lowerBridge ls rs

upperBridge, lowerBridge      :: (Ord a, Num a) => [P2 a] -> [P2 a] -> (P2 a,P2 a)
upperBridge ls rs             = bridge isLeftTurn (reverse ls) rs
lowerBridge ls rs             = bridge isRightTurn ls (reverse rs)

bridge                        :: (Ord a, Num a)=> Rel3 (P2 a) -> [P2 a] -> [P2 a] -> (P2 a, P2 a)
bridge turn ls rs             = find (rotate rightest ls) (rotate leftest rs)
  where rotate f xs	      = cycle (rotateTo (f xs) xs)
        find xs@(l:xxs@(m:_)) ys@(r:yys@(n:_))
           | turn l r m       = find xxs ys
           | turn l r n       = find xs yys
           | otherwise        = (l, r)
\end{code}

Mit der Funktion |merge| läßt sich ein inkrementeller Algorithmus mit Laufzeit $O(n^2)$
entwerfen.

\begin{code}
insert                        :: (Fractional a, Ord a) => [P2 a] -> P2 a -> [P2 a]
insert [] p                   = [p]
insert ch p                
  | p `inConvex` (PolygonCW ch) = ch
  | otherwise                 = merge ch [p]

incrementalMergeHull          :: (Fractional a, Ord a) => [P2 a] -> Polygon2 a
incrementalMergeHull          = PolygonCW . deleteCollinear 
			      . foldl insert [] 
			      . concat . filterAndSort
\end{code}

Auch eine bottomup-Version läßt sich schreiben.

\begin{code}
bottomupMergeHull             :: (Num a, Ord a) => [P2 a] -> Polygon2 a
bottomupMergeHull             = PolygonCW . deleteCollinear . head 
			      . merges . initial . filterAndSort

merges			      :: (Ord a, Num a) => [[P2 a]] -> [[P2 a]]
merges []		      = []
merges [x]		      = [x]
merges (x:y:ys)		      = merges (merge x y : merges ys)

initial			      :: (Ord a, Num a) => [[P2 a]] -> [[P2 a]]
initial []		      = []
initial [p]		      = [p]
initial ([p]:[q]:xs)	      = initial ([p,q]:xs)
initial ([p,q]:[r]:xs)	      = clockwise3 [p,q,r] : initial xs
initial ([p]:[q,r]:xs)	      = clockwise3 [p,q,r] : initial xs
initial ([p,q]:[r,s]:xs)      = [p,q] : initial ([r,s]:xs)
\end{code}


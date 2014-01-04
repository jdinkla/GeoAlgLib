%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Anwendung: Dichteste Punktepaare (\texttt{ClosestPair})}
\module{ClosestPair}

siehe \cite[K. 2.3.1, K.2.4]{klein97:cg}

\begin{code}
module Applications.ClosestPair (
         naiveMinSqrDistance, naiveClosestPair, naiveClosest,
         minSqrDistance, closestPair, closest
       ) where

import Point     ( Point2, P2, lessX, xcoord, Point ((</=>), (<==>), mapP), sqrDistance, lexic )
import Basics.Sorting   ( sortBy )
import Basics.Utilities ( relToSnd, relToFst, minimumWith, minimumBy)
import RangeTree
import List      ( deleteBy )
\end{code}

\begin{code}
naiveClosest                  :: (Point p, Ord a, Num a) => [p a] -> (a, (p a, p a))
naiveClosest ps		      = minimumBy (relToFst (<=)) (pairs ps)
  where pairs []	      = []
        pairs (x:xs)	      = [ (sqrDistance x y, (x,y)) | y <- xs] ++ pairs xs

naiveMinSqrDistance	      :: (Point p, Ord a, Num a) => [p a] -> a
naiveMinSqrDistance	      = fst . naiveClosest

naiveClosestPair              :: (Point p, Ord a, Num a) => [p a] -> (p a, p a)
naiveClosestPair              = snd . naiveClosest
\end{code}

Im wesentlichen der Algorithmus aus \cite[K. 2.3]{klein97:cg}

\begin{code}
closest	                      :: (Point p, Ord a, Num a) => [p a] -> (a, (p a,p a))
closest ps                    = sweep ls rs (sqrDistance p q, (p,q))
  where 
  ls@(p:q:rs)                 = sortBy lessX ps
  rtree                       = fromList ps
  sweep _ [] m                = m
  sweep xs@(l:ls) ys@(r:rs) m@(min, _)
    | xcoord r-xcoord l >=min = sweep ls ys m
    | null range || min'>=min = sweep xs rs m
    | otherwise               = sweep xs rs (min', (y,r))
    where range               = deleteBy (<==>) r (rangeQuery rtree (p,q))
	    where (p,q)       = (mapP ((+)(-min)) r, mapP (+min) r)
          y 		      = minimumWith (sqrDistance r) range
	  min'		      = sqrDistance r y

minSqrDistance		      :: (Point p, Ord a, Num a) => [p a] -> a
minSqrDistance		      = fst . closest

closestPair		      :: (Point p, Ord a, Num a) => [p a] -> (p a, p a)
closestPair		      = snd . closest
\end{code}






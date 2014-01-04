%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Alle nächsten Nachbarn (|AlNearest|)}
\module{AllNearest}

siehe \cite[K 5.3]{klein97:cg},
\cite[K. 5.5]{orourke94:cg}.

\begin{code}
module Applications.AllNearest where {-( allNearest, closestPair, 
		    minSqreDistances, adjacent, adjacent', adjacent'' ) where-}

import QEDSbasics ( Direction (..), Orientation (..), EdgeRef, Edge (..) )
import QEDSstatic ( sym, onext, ring, connected, nodes )
import qualified  QEDSstatic as S
import Delaunay   ( delaunay, DelEdge (..) )
import Point2     ( P2, leftOrBelow, sqrDistance, lexic2, Point2 (..) )
import Basics.Utilities  ( relToFst, minimumBy, fst3 )
import Basics.Sorting    ( sortBy )
\end{code}

|allNearest| ermittelt für jeden Punkt seinen nächsten Nachbarn

\begin{code}
allNearest                    :: (Ord a, Num a) => [P2 a] -> [(a, (P2 a, P2 a))]
allNearest                    = minSqrDistances . adjacent . fst3 . delaunay

minSqrDistances		      :: (Ord a, Num a) => [(P2 a, [P2 a])] -> [(a, (P2 a, P2 a))]
minSqrDistances               = map min 
  where min (p, qs)	      = (m, (p,q))
	  where (m, q)	      = minimumBy (relToFst (<)) [(sqrDistance p q, q) | q<-qs]

closestPair		      :: (Ord a, Num a) => [P2 a] -> (a, (P2 a, P2 a))
closestPair   		      = minimumBy (relToFst (<)) . allNearest 
\end{code}

|adjacent| bestimmt die Adjazensliste einer QEDS.

\begin{code}
adjacent   		      :: (Ord a, Num a) => S.QEDS (DelEdge (P2 a)) -> [(P2 a,[P2 a])]
adjacent q		      = map mkPair (nodes Rot0 q)
  where mkPair e	      = (org q e, map (dest q) (ring q onext e))

org, dest                     :: S.QEDS (DelEdge a) -> EdgeRef -> a
org q x@(i, r, _)             = f r (S.getAttr q x)
  where f Rot0                = source
        f Rot2                = target
dest q x@(i, r, _)            = f r (S.getAttr q x)
  where f Rot2                = source
        f Rot0                = target
\end{code}

Das funktionale |adjacent'| ist $O(n\log n)$

\begin{code}
{-
adjacent'		      :: (Ord a, Num a) => S.QEDS (P2 a) f a -> [(P2 a,[P2 a])]
adjacent'		      = collect . adjacentPairs . allEdges
  where collect xs	      = map pack xs
	pack xs@(x:_)	      = (fst x, map snd xs)
	pack _		      = error "adjacent"

adjacentPairs		      :: (Ord a, Num a) => [(P2 a, P2 a)] -> [[(P2 a, P2 a)]]
adjacentPairs		      = groupBy (relToFst (==)) 
			      . sortBy (relToFst leftOrBelow) 

allEdges		      :: S.QEDS p f a -> [(p,p)]
allEdges q	      	      = es ++ fs
  where es		      = edges (map snd (connected q))
	fs		      = map (uncurry (flip (,))) es
-}
\end{code}

|adjacent''| ist $O(n)$, ermittelt aber die nächsten Nachbarn mehrfach.

\begin{code}
adjacent'	              :: (Ord a, Num a) => S.QEDS (DelEdge (P2 a)) -> [(P2 a,[P2 a])]
adjacent' q		      = concatMap neighbours edges
  where edges		      = map ( \ (i,_) -> (i, Rot0, Normal)) (connected q)
	neighbours e	      = [(org q e, os), (dest q e, ds)]
	  where	os	      = map (dest q) (ring q onext e)
		ds	      = map (dest q) (ring q onext (sym e))
\end{code}


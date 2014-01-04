%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Chans Algorithmus (|ChansConvexHull|)}
\module{ChansConvexHull}

nach \cite{chan96:convex23}

\begin{code}
module ChansConvexHull (
         hullOfHulls, genericChan, chan 
       )
where

import GrahamsScan ( graham2 )
import JarvisMarch ( withMinimalSlopeInt )
import Point2      ( Point2 (..), P2, isLeftTurnOrOn, 
		     rightestLowest, isRightTurnOrNearer, cclockwise3 )
import Polygon	   ( Polygon ( PolygonCCW ), Polygon2, vertices )
import Basics.Utilities   ( splitsAt, maximumBy, rotateTo, longerThan )

advanceTo		      :: (Num a, Ord a) => Point2 a -> [[Point2 a]] -> [[Point2 a]]
advanceTo p 		      = map (until leftTurn tail)
  where leftTurn (s:t:_)      = s==t || (s/=p && isLeftTurnOrOn p s t)

maximumWrt		      :: (Ord a, Num a) => Point2 a -> [[Point2 a]] -> Point2 a
maximumWrt p		      = maximumBy (isRightTurnOrNearer p) . map head

hullOfHulls                   :: (Ord a, Num a) => [Polygon2 a] -> Polygon2 a
hullOfHulls polys	      = PolygonCCW (march p0 p0 ws)
  where ps		      = map vertices polys
	ls		      = map rightestLowest ps
	p0		      = rightestLowest ls
	ws		      = zipWith (\ x xs -> cycle (rotateTo x xs)) ls ps

march                         :: (Ord a, Num a) => P2 a -> P2 a -> [[P2 a]] -> [P2 a]
march p0 x ws
          | q == p0	      = [x]
	  | otherwise	      = x : march p0 q ws'
	  where ws'	      = advanceTo x ws
		q	      = maximumWrt x ws'

genericChan		      :: (Ord a, Num a) => [P2 a] -> [(Int,Int)] -> Polygon2 a
genericChan ps@(_:_:_:_) is   = try ps is
  where try xs ((h,m):is)     
          | vs `longerThan` h = try (concatMap vertices polys) is
	  | otherwise	      = hull
          where
          polys		      = map graham2 (splitsAt m xs)
	  hull	              = hullOfHulls polys
	  vs		      = vertices hull
genericChan ps _              = PolygonCCW (cclockwise3 ps)

chan			      :: (Ord a, Num a) => [P2 a] -> Polygon2 a
chan ps		              = genericChan ps (zip is is)
  where is 		      = [ min (2^(2^t)) n  | t <- [1..] ]
        n                     = length ps
\end{code}


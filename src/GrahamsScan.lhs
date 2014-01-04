%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Grahams Scan (|GrahamsScan|)}
\module{GrahamsScan}

\begin{code}
module GrahamsScan ( 
         graham, graham2, graham3, graham4
       )
where

import Point2          ( Point2, Point2D, rightestLowest, leftestLowest,
			 leftOrBelow, isLeftTurn, isLeftTurnOrNearer, isLeftTurnOrOn )
import Polar           ( leq, toPolar )
import Polygon         ( Polygon (PolygonCW), Polygon2 )
import Basics.Sorting  ( sortBy, sortMapBy )

graham			      :: [Point2 Double] -> Polygon2 Double
graham ps		      = PolygonCW (init (leftTurns [s, last xs] ss))
  where xs@(s:ss)	      = sortByAngle ps

sortByAngle                   :: [Point2 Double] -> [Point2 Double]
sortByAngle ps@(_:_)          = sortMapBy leq (\ q -> toPolar (q-p)) ps
  where p                     = rightestLowest ps

leftTurns		      :: (Ord a, Num a) => [Point2 a] -> [Point2 a] -> [Point2 a]
leftTurns		      = foldl deleteRightTurns

deleteRightTurns              :: (Ord a, Num a) => [Point2 a] -> Point2 a -> [Point2 a]
deleteRightTurns [x,y] p      = [p,x,y]
deleteRightTurns xs@(s1:s2:ss) p
  | isLeftTurn s2 s1 p        = p:xs 
  | otherwise		      = deleteRightTurns (s2:ss) p
\end{code}

Die Ganzzahlversion: |graham2|.

\begin{code}
graham2			      :: (Ord a, Num a) => [Point2 a] -> Polygon2 a
graham2 ps		      = PolygonCW (init (leftTurns [s, last xs] ss))
  where xs@(s:ss)	      = sortByOrientation ps

sortByOrientation             :: (Num a, Ord a) => [Point2 a] -> [Point2 a]
sortByOrientation ps          = sortBy (isLeftTurnOrNearer p) ps
  where p                     = rightestLowest ps
\end{code}

Die Version aus \cite[S. 6-8]{berg97:cg}.

\begin{code}
graham3                       :: (Ord a, Num a) => [Point2 a] -> Polygon2 a
graham3 xs@(_:_:_)            = PolygonCW (init (tail lowerHull) ++ upperHull)
  where 
  sorted@(p1:p2:ps)	      = sortBy leftOrBelow xs
  (q1:q2:qs)		      = reverse sorted
  upperHull		      = insert [q2,q1] qs
  lowerHull		      = insert [p2,p1] ps
  insert		      = foldl delete
  delete [x] p		      = [p,x]
  delete xs@(s1:s2:ss) p
    | isLeftTurn s2 s1 p      = p:xs 
    | otherwise		      = delete (s2:ss) p
graham3 xs		      = PolygonCW xs
\end{code}

|graham4| gibt die kollinearen Punkte aus.

\begin{code}
graham4                       :: (Ord a, Num a) => [Point2 a] -> Polygon2 a
graham4 xs@(_:_:_)            = PolygonCW (init (tail lowerHull) ++ upperHull)
  where 
  sorted@(p1:p2:ps)	      = sortBy leftOrBelow xs
  (q1:q2:qs)		      = reverse sorted
  upperHull		      = insert [q2,q1] qs
  lowerHull		      = insert [p2,p1] ps
  insert		      = foldl delete
  delete [x] p		      = [p,x]
  delete xs@(s1:s2:ss) p
    | isLeftTurnOrOn s2 s1 p  = p:xs 
    | otherwise		      = delete (s2:ss) p
graham4 xs		      = PolygonCW xs
\end{code}


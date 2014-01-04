%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%

Berechne die Konvexe Hülle mit Hilfe der Delaunay-Triangulation.

\begin{code}
module Applications.DelaunayHull (delaunayHull) where

import Point2   ( Point2 )
import Polygon  ( Polygon2, Polygon (..) )
import Delaunay ( delaunay )
import qualified QEDSstatic as S
\end{code}

\begin{code}
delaunayHull	              :: (Ord a, Num a) => [Point2 a] -> Polygon2 a
delaunayHull ps			  = undefined
{-
delaunayHull ps		      = PolygonCCW (map (\ e -> S.org q e) es)
  where (q,l,r)		      = delaunay ps
	es		      = S.ring q S.rprev l
	-}
\end{code}
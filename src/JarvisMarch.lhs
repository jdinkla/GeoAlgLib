%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
% 
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Jarvis March (|JarvisMarch|)}
\module{JarvisMarch}

\begin{code}
module JarvisMarch ( 
         jarvis, withMinimalSlope, withLargestAngle,
	 jarvisInt, withMinimalSlopeInt, withLargestAngleInt
       )
where

import Point2    ( Point2 (..), P2, Point2D, 
		   rightestLowest, sqrDistance, angleWrt, angle3,
		   isCollinear, isRightTurnOrNearer )
import Polygon   ( Polygon (PolygonCW), Polygon2 )
import Data.List      ( (\\) )
import Basics.Utilities ( maximumWith, maximumBy )
\end{code}

siehe \cite[K. 4.2.1]{chen96:cg}, \cite[K. 3.3]{orourke94:cg},
\cite[K. 35.3]{cormen90:introduction}).

\begin{code}
genericJarvis		      :: (Ord a, Num a) => (P2 a -> P2 a -> [P2 a] -> P2 a) 
			      -> (P2 a -> [P2 a] -> P2 a) -> [P2 a] -> Polygon2 a
genericJarvis angle slope     = generic
  where
  generic xs@(_:_:_:_)        = PolygonCW (march [p1,p0])
    where march               = tail . until stop add
	  p0                  = rightestLowest xs 
	  p1                  = slope p0 xs
	  stop (q:s:t:_)      = isCollinear t s q || q == p0 
	  stop _	      = False
	  add hull@(s:t:_)    = angle t s xs : hull
  generic xs                  = PolygonCW xs
\end{code}

Die Integer-Version ist nicht unbedingt korrekter, siehe points2 10009 200.

\begin{code}
jarvis                        :: (Floating a, Ord a) => [P2 a] -> Polygon2 a
jarvis			      = genericJarvis withLargestAngle withMinimalSlope

withMinimalSlope              :: (Floating a, Ord a) => P2 a -> [P2 a] -> P2 a
withMinimalSlope p ps         = withLargestAngle (p-(Point2 (1,0))) p ps

withLargestAngle              :: (Floating a, Ord a) => P2 a -> P2 a -> [P2 a] -> P2 a
withLargestAngle p q ps       = maximumWith key (ps \\ [p,q])
  where key x                 = (angle3 p q x, sqrDistance q x)

jarvisInt		      :: (Ord a, Num a) => [P2 a] -> Polygon2 a
jarvisInt		      = genericJarvis withLargestAngleInt withMinimalSlopeInt

withMinimalSlopeInt	      :: (Ord a, Num a) => P2 a -> [P2 a] -> P2 a
withMinimalSlopeInt p ps      = withLargestAngleInt p p ps

withLargestAngleInt	      :: (Ord a, Num a) => P2 a -> P2 a -> [P2 a] -> P2 a
withLargestAngleInt p q ps    = maximumBy (isRightTurnOrNearer q) (ps \\ [q])
\end{code}


%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Ein naiver $O(n^3)$ Algorithmus (|NaiveTriangulation|)}
\module{NaiveTriangulation}

\begin{code}
module NaiveTriangulation ( 
         earExcision, diagonal, triangulateBy, naiveTri
       )
where

import Point2             ( Point2, P2, isLeftTurn, isLeftTurnOrOn )
import Line		  ( Line (..), Line2, doIntersect, sqrDistanceFromLine )
import Polygon		  ( Polygon, Polygon2, edges, vertices, angles )
import Triangle		  ( Triangle (..), Triangle2, containsBNV )
import Data.List               ( delete, (\\), zip4 )
import Basics.Utilities          ( maximumWith, uncurry3, split, rotateL, rotateR )
import Basics.ListCat
\end{code}

Implementierung des Lemmas.

\begin{code}
type Diagonal a		      = (Point2 a, Point2 a, Point2 a, Point2 a)

isDiagonal                    :: (Ord a, Fractional a) => Diagonal a -> [P2 a] -> Bool
isDiagonal d ps               = inCone d && noIntersection d ps

noIntersection                :: (Ord a, Fractional a) => Diagonal a -> [P2 a] -> Bool
noIntersection (_,i,_,j) ps   = all (not . intersect) es
  where es                    = filter (not . incident) (edges ps)
        incident (k,l)        = (k==i) || (l==i) || (k==j) || (l==j) 
        intersect (k,l)       = doIntersect (Segment i j) (Segment k l)

inCone                        :: (Ord a, Num a) => Diagonal a -> Bool
inCone (m,i,p,j)
  | isLeftTurnOrOn m i p      = isLeftTurn i j m && isLeftTurn j i p
  | otherwise                 = not ( isLeftTurnOrOn i j p && isLeftTurnOrOn j i m )
\end{code}

In der Arbeit benutzten wir folgende Version zur Herleitung, in der die Liste der Kandidaten
expliziert generiert wird.

\begin{code}
candidates                     :: [Point2 a] -> [Diagonal a]
candidates xs@(_:_:_:_)	       = zip4 (rotateR xs) xs ys (rotateL ys)
  where ys 		       = rotateL xs
candidates xs		       = []

earExcision                   :: (Ord a, Fractional a) => Polygon2 a -> [Triangle2 a]
earExcision                   = triangulate . vertices
  where
  triangulate [i,p,j]         = [Triangle (i,p,j)]
  triangulate ps@(_:_:_:_:_)  = Triangle (i,p,j) : triangulate (delete p ps)
    where ((m,i,p,j):_)       = filter (\ x -> isDiagonal x ps) (candidates ps)
\end{code}

Und dieses ist die direkte Implementierung eines konstruktiven Beweises, daß jedes einfache
Polygon trianguliert werden kann.

\begin{code}
naiveTri                      :: (Ord a, Fractional a) => Polygon2 a -> [Triangle2 a]
naiveTri		      = triangulateBy diagonal

diagonal		      :: (Ord a, Fractional a) => [P2 a] -> Line2 a
diagonal ps | null inTriangle = Segment m p
	    | otherwise	      = Segment i j
  where x@(m,i,p)	      = findConvexVertex ps
	inTriangle	      = filter (containsBNV (Triangle x)) ps
	j		      = maximumWith (sqrDistanceFromLine (Line m p)) inTriangle

triangulateBy                 :: (Num a, Eq a) => ([P2 a] -> Line2 a) -> Polygon2 a -> [Triangle2 a]
triangulateBy diag            = tri . vertices
  where
  tri [x,y,z]                 = [Triangle (x,y,z)]
  tri ps@(_:_:_:_:_)          = tri ls ++ tri rs
    where (Segment p q)       = diag ps
          (ls, rs)            = split p q ps

triangulateBy'                 :: (Num a, Eq a) => ([P2 a] -> Line2 a) -> Polygon2 a -> [Triangle2 a]
triangulateBy' diag            = list . tri . vertices
  where
  tri [x,y,z]                 = unit (Triangle (x,y,z))
  tri ps@(_:_:_:_:_)          = (tri ls) `cat` (tri rs)
    where (Segment p q)       = diag ps
          (ls, rs)            = split p q ps

findConvexVertex              :: (Ord a, Num a) => [P2 a] -> (P2 a, P2 a, P2 a)
findConvexVertex              = head . filter (uncurry3 isLeftTurn) . angles
\end{code}

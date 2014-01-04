%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Polygone (|Polygon|)}
\module{Polygon}

\begin{code}
module Polygon (
         Polygon (..), Polygon2, Polygon3, mapPolygon, 
	 vertices, edges, angles,
	 area, contains, isConvex, 
	 deleteCollinear,
	 extreme, extremeArr,
         naiveInConvex, inConvexInteriour, inConvex
       )
where

import Point	 ( Point ((<==>), (<.>), (<*>)), Point3 )
import Point2    ( Point2 (..), P2, Point (origin), isOn, isRightTurnOrOn, 
		   isCollinear, isLeftTurnOrOn, xcoord, ycoord, isOn )
import Line      ( Line (..), fromPDL, distanceFromLine, intersect, doIntersect )
import Basics.Utilities ( rotateL, rotateR, rotateTo, snd3, uncurry3, rotateToBy )
import qualified Triangle
import Data.Array	 ( Array, listArray, (!) )
\end{code}

\begin{code}
data (Point p, Num a)         => Polygon p a 
			      = PolygonCW [p a] 
			      | PolygonCCW [p a]

type Polygon2 a		      = Polygon Point2 a
type Polygon3 a		      = Polygon Point3 a

--polygon			      :: (Ord a, Num a) => [P2 a] -> Polygon p a
--polygon			      = PolygonCW . deleteCollinear

mapPolygon		      :: (Point p, Num a, Num b, Eq a, Eq b) => (p a -> p b) 
			      -> Polygon p a -> Polygon p b
mapPolygon f (PolygonCW xs)   = PolygonCW (map f xs)
mapPolygon f (PolygonCCW xs)  = PolygonCCW (map f xs)

vertices                      :: (Point p, Num a) => Polygon p a -> [p a]
vertices (PolygonCW ps)       = reverse ps
vertices (PolygonCCW ps)      = ps

edges                         :: [a] -> [(a,a)]
edges xs                      = zip xs (rotateL xs)

angles                        :: [a] -> [(a,a,a)]
angles xs                     = zip3 (rotateR xs) xs (rotateL xs)

instance (Eq a, Num a, Point p) => Eq (Polygon p a) where
-- x == y                     = ys == rotateTo (head ys) xs
   x == y                     = and (zipWith (<==>) ys (rotateToBy (<==>) (head ys) xs))
     where xs                 = vertices x 
	   ys		      = vertices y
\end{code}


Die Fläche eines Polygons wird nach Theorem 1.4.3 aus
\cite{orourke94:cg} berechnet.

\begin{code}
area                          :: (Fractional a, Eq a) => Polygon2 a -> a

area poly                     = sumTri ps
    where sumTri (q:r:rest)   = Triangle.area (Triangle.Triangle (p,q,r)) + sumTri (r:rest)
          sumTri _            = 0
          (p:ps)              = vertices poly
\end{code}

\texttt{countCrossings} ist eine erweiterte Version der Funktion
\texttt{InPoly} aus \cite[S. 235]{orourke94:cg}. Die auf Seite 236
beschriebenen Nachteile werden durch Extratests umgangen.

\begin{code}
contains                      :: (Num a, Ord a) => Polygon2 a -> P2 a -> Bool
contains poly p               = eq>0 || odd pos || odd neg
    where (pos, eq, neg)      = countCrossings (edges qs) 0 0 0 
          qs                  = map ((-)p) (last ps : init ps)
          ps		      = vertices poly

countCrossings                :: (Ord a, Num a) => [(P2 a, P2 a)] 
                                 -> Int -> Int -> Int -> (Int, Int, Int)  
countCrossings [] cp ce cn    = (cp, ce, cn)
countCrossings ((p@(Point2 (x',y')), q@(Point2 (x,y))):ps) cp ce cn
  | straddlesXaxis            = incr
  | straddlesFromBelow || p==origin || q==origin
                              = countCrossings ps cp (ce+1) cn
  | otherwise                 = countCrossings ps cp ce cn
  where straddlesXaxis        = (y>0 && y'<=0) || (y'>0 && y<=0) 
        straddlesFromBelow    = (y<=0 && y'==0) && (y==0 && y'<=0) 
                                && ((x<=0 && x'>=0) || (x>=0 && x'<=0))
        incr | sgn>0          = countCrossings ps (cp+1) ce cn 
             | sgn==0         = countCrossings ps cp (ce+1) cn
             | sgn<0          = countCrossings ps cp ce (cn+1)
             where sgn        = signum (x*y' - x'*y) * signum (y' - y)
\end{code}

\begin{code}
isConvex		      :: (Ord a, Num a) => Polygon2 a -> Bool
isConvex     		      = all (uncurry3 isLeftTurnOrOn) . angles . vertices

deleteCollinear               :: (Ord a, Num a) => [P2 a] -> [P2 a]
deleteCollinear xs@(_:_:_:_)  = [ i | (m,i,p)<-angles xs, not (isCollinear m i p)]
deleteCollinear xs	      = xs
\end{code}

%
% 
%
\subsubsection{Extremale Punkte eines konvexen Polygons}

|extreme| ermittelt den extremalen Punkt des konvexen Polygons |p| bezüglich des
Richtungsvektors |u| (aus \cite[K. 7.3]{orourke94:cg}).

\begin{code}
extreme                       :: (Num a, Ord a) => P2 a -> Polygon2 a  -> P2 a
extreme u p	              = arr ! extremeArr arr u n
  where ps		      = vertices p
	n                     = length ps
        arr                   = listArray (0,n-1) ps

extremeArr		      :: (Num a, Ord a) => Array Int (P2 a) -> P2 a -> Int -> Int
extremeArr p u n              = search 0 0
  where 
  ur                          = Point2 (ycoord u, - xcoord u)
  search a b 
    | adot==0 && ur<.>vecA<0  = a
    | cdot==0 && ur<.>vecC<0  = c
    | cdot<0 && bdot>0        = c
    | a==c                    = if adot>0 then b else a
    | otherwise               = goOn
    where
    goOn
      | adot>=0 && cdot<=0    = search a c
      | adot<=0 && cdot>=0    = search c b
      | adot>0 && cdot>0      = if y>0 then search a c else search c b
      | adot<0 && cdot<0      = if y<0 then search a c else search c b
    c | a < b		      = (a+b) `div` 2
      | otherwise	      = (a+b+n) `div` 2 `mod` n 
    vecA		      = p!a1 - p!a where a1 = (a+1) `mod` n
    vecC		      = p!c1 - p!c where c1 = (c+1) `mod` n 
    vecB		      = p!c - p!c2 where c2 = (c+(n-1)) `mod` n 
    adot		      = u <.> vecA
    cdot		      = u <.> vecC
    bdot		      = u <.> vecB
    y			      = (u <.> p!a) - (u <.> p!c)
\end{code}

%
% SCHNITTPUNKTTESTS
%
\subsubsection{Schnittpunkttests}

Für konvexe Polygone ist der Test auf Enthaltensein einfach, zuerst die naive Methode, die
testet, ob sich der Punkt links von oder auf allen Kanten befindet.

\begin{code}
naiveInConvex                :: (Num a, Ord a) => P2 a -> Polygon2 a -> Bool
q `naiveInConvex` p           = case vs of 
                                  [] -> False
                                  [s] -> q==s
                                  [s,t] -> isOn s t q
                                  ps -> all ok (edges vs)
  where vs	              = vertices p
	ok (s,t)	      = isLeftTurnOrOn s t q
\end{code}

Der Punkt |p| ist genau dann im Inneren eines Polygon enthalten, wenn die Strecke zwischen |p|
und einem Punkt |c| im Innern von |p| keinen Schnittpunkt mit dem Rand von |p| bei einer Kante
|e| hat, die inzident zum extremalen Eckpunkt von |p| in Richtung |p-c| ist.

\begin{code}
inConvexInteriour	      :: (Fractional a, Ord a) => P2 a -> Polygon2 a -> Bool
p `inConvexInteriour` poly    = not (doIntersect (Segment qm q) (Segment c p) 
			      || doIntersect (Segment q qp) (Segment c p))
  where ps@(a:_:b:_)	      = vertices poly
	n		      = length ps
	arr		      = listArray (0,n-1) ps
	c		      = 0.5 <*> (a+b)
	iq		      = extremeArr arr (p-c) n
	qm		      = arr ! ((iq-1) `mod` n)
	q		      = arr ! iq
	qp		      = arr ! ((iq+1) `mod` n)

inConvex	              :: (Fractional a, Ord a) => P2 a -> Polygon2 a -> Bool
p `inConvex` poly             = not (doIntersect (Segment qm q) (Segment c p) 
			      || doIntersect (Segment q qp) (Segment c p))
			      || (isLeftTurnOrOn qm q p && isLeftTurnOrOn q qp p)
  where ps@(a:_:b:_)	      = vertices poly
	n		      = length ps
	arr		      = listArray (0,n-1) ps
	c		      = 0.5 <*> (a+b)
	iq		      = extremeArr arr (p-c) n
	qm		      = arr ! ((iq-1) `mod` n)
	q		      = arr ! iq
	qp		      = arr ! ((iq+1) `mod` n)
\end{code}

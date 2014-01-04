%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Ein ausgabesensitiver Algorithmus (|AdaptTriangulation|)}
\module{AdaptTriangulation}

\begin{code}
module AdaptTriangulation (
         adaptTri, diagonal
       )
where

import Point2    ( Point2 (..), P2, sqrDistance, rotate, angleWrt, angle3, angle2,
		   orientation, Orientation (..), isLeftTurn, isRightTurn )
import Line      ( Line (..), Line2, L2, centerOfSegment, intersect, doIntersect)
import Polygon   ( Polygon (PolygonCW), Polygon2, angles, edges, vertices, 
		   deleteCollinear, mapPolygon )
import Maybe     ( fromJust, catMaybes, mapMaybe, isJust )
import Triangle  ( Triangle (..), Triangle2, contains, inInteriourOf, containsBNV )
import List      ( elemIndex )
import Array     ( Array, listArray, (!) )
import Basics.ListCat
import Basics.Utilities ( uncurry3, minimumWith, sublist2, minimumBy, sublist, splitWhile)
\end{code}

Der Name searchTriangles ist schlecht !

nach \cite{toussaint91:simplepolygons}

S. 10-12

\begin{code}
type Angle a		      = (P2 a, P2 a, P2 a)

diagonal                      :: (Ord a, Floating a) => [P2 a] -> Line2 a
diagonal ps		      = case searchTriangles (m,i,p) ps of
				  Nothing -> Segment m p
				  Just q -> Segment i q
  where (m,i,p)		      = findConvexVertex ps

findConvexVertex              :: (Ord a, Num a) => [P2 a] -> (P2 a, P2 a, P2 a)
findConvexVertex              = head . filter (uncurry3 isLeftTurn) . angles

searchTriangles               :: (Floating a, Ord a) => Angle a -> [P2 a] -> Maybe (P2 a)
searchTriangles x@(m,i,p) ps
  | y == a || y == b          = Just y
  | isJust inLeft && l /= m   = inLeft
  | isJust inRight && r /= p  = inRight
  | otherwise                 = Nothing
  where ray                   = bisectInteriorAngle x
	ps'		      = sublist p m ps
        (a, y, b)             = intersection ray x ps'
        inLeft                = vertexInTriangle (i,y,b) (sublist b m ps')
	(Just l)	      = inLeft
        inRight               = vertexInTriangle (y,i,a) (sublist p a ps')
	(Just r)	      = inRight

bisectInteriorAngle           :: (Ord a, Floating a) => Angle a -> Line2 a
bisectInteriorAngle (m,i,p)
--  | phi > 0                   = Ray i (rotate m i (-(2*pi-phi)/2))
  | phi > 0                   = Ray i (rotate m i (phi/2 - pi))
--  | phi == 0		      = Ray i (i - Point2 (-1,0))
  | phi == 0		      = Ray i (i + Point2 (1,0))
  | phi < 0		      = Ray i (rotate m i (phi/2))
  where phi		      = angle3 p i m

intersection                  :: (Ord a, Fractional a) => L2 a -> Angle a -> [P2 a] -> Angle a
intersection ray (m,i,p) ps   = minimumWith (\ (_,y,_) -> sqrDistance i y) is
  where 
    is                        = mapMaybe inters (init (edges ps))
    inters (i,j)
      | isJust res            = Just (i,y,j)
      | otherwise             = Nothing
      where
        res                   = intersect ray (Segment i j)
        y                     = fromJust res
    
vertexInTriangle	      :: (Ord a, Num a) => Angle a -> [P2 a] -> Maybe (P2 a)
vertexInTriangle t@(p,q,r) xs = case filter (Triangle t `contains`) xs of 
				  [] -> Nothing
				  ys -> Just (minimumBy (isLeftTurn p) ys)
\end{code}

Wir benötigen folgende Hilfsfunktionen:

S.15, |Wedge p q r| ist der Teil der Ebene, der ``rechts'' durch den von |p| ausgehenden Strahl
durch |q| und ``rechts'' durch |r| begrenzt wird. Wir benötigen einen Test, ob eine Strecke in
einem Keil enthalten ist.

\begin{code} 
inWedge                       :: (Ord a, Floating a) => (P2 a,P2 a) -> (P2 a,P2 a,P2 a) -> Bool
(a,b) `inWedge` (p,q,s)
  | r > 0		      = not ( (0<i && i<r) || (0<j && j<r) )
  | r < 0		      = i<=0 && i>r && j<=0 && j>r
  | r == 0		      = True
  where r		      = angleWrt p q s
	i		      = angleWrt p q a
	j		      = angleWrt p q b
\end{code}

\begin{code}
adaptTri		      :: (Floating a, Ord a) => Polygon2 a -> [Triangle2 a]
adaptTri poly		      = triangulate ds
  where
  ds			      = [(i,j), (j,i)]
      where i		      = fromJust (elemIndex xi ps)
	    j		      = fromJust (elemIndex xj ps)
	    (Segment xi xj)   = diagonal ps

  triangulate []              = []
  triangulate ((i,j):ds)      = map snd ts ++ triangulate (ds' ++ ds)
    where (ds', ts)           = sleeve i j []

  ps			      = vertices poly
  n			      = length ps
  arr			      = listArray (0,n-1) ps
  pred i		      = (i-1) `mod` n
  succ i		      = (i+1) `mod` n
  angle i		      = (arr!(pred i), arr!i, arr!(succ i))

  sleeve i j ts
    | i==j || i==succ j || j==succ i = ([], ts)
    | (xi,xip) `inWedge` (xi,xj,xim) && (xj,xjm) `inWedge` (xj,xjp,xi) 
			      = turns (orientation xjm xj xi) (orientation xip xi xj)
    | otherwise		      = freeTriangle
    where
    (xim,xi,xip)	      = angle i
    (xjm,xj,xjp)	      = angle j
    inBetween	              = sublist xip xjm ps
    index x		      = (i + 1 + (fromJust (elemIndex x inBetween))) `mod` n

    turns Clockwise Clockwise                    = nexti  
    turns Collinear Clockwise			 = nexti
    turns CounterClockwise CounterClockwise	 = prevj       
    turns CounterClockwise Collinear		 = prevj
    turns CounterClockwise Clockwise   
      | xjm `inInteriourOf` (Triangle (xip,xi,xj)) = prevj
      | otherwise				 = nexti
    turns _ _
      | null wrongTriangles			 = newFreeTriangle
      | otherwise				 = freeTriangle

    nexti		      = sleeve (succ i) j (((j,i,succ i), Triangle (xj,xi,xip)) : ts)
    prevj		      = sleeve i (pred j) (((j,i,pred j), Triangle (xj,xi,xjm)) : ts)

--    (ts', wrongTriangles)     = splitWhile (not . inInt) (reverse ts)
--      where inInt (_,x)	      = any (x `containsBNV`) inBetween

    (ts', wrongTriangles)	
      | (xi,xip) `inWedge` (xi,xj,xim)  = findWrong ts
      | otherwise             = let (vs, w:ss) = splitWhile notInTri ts
				    (xs, ws) = findWrong ss 
				in (xs, ws ++ [w] ++ reverse vs)
      where
      notInTri (_,t)          = not (t `containsBNV` xip)
      findWrong ts 	      = foldl delete (ts, []) (zip inBetween (tail inBetween))
      delete (ts, ts') (p, q) = (ds, reverse cs ++ ts')
        where 
        (cs, ds)              = splitWhile (doIntersect (Segment p q) . diag) ts
        diag ((j,i,n), Triangle (xj,xi,xn)) 
	  | n == succ i       = Segment xn xj
  	  | n == pred j       = Segment xi xn

    -- nach Lemma 5
    freeTriangle 
      | null wrongTriangles   = ([], ts)
      | otherwise	      = ([(s,k),(k,r)], (undefined, Triangle (xr,xs,xk)) : ts')
      where
      ((r,s,t), tri@(Triangle (xr,xs,xt))) = head wrongTriangles
      inTriangle	      = filter (tri `containsBNV`) inBetween
      xk		      = minimumWith (key xs xr) inTriangle
      k			      = index xk

    key p q x		      = (angle3 x q p, sqrDistance q x)
   
    -- nach Lemma 2+3
    newFreeTriangle	      = ([(i,l),(l,j)], (undefined, Triangle (xj,xi,xl)) : ts)
      where
      xk		      = fromJust (searchTriangles (xj,xi,xip) inBetween)
      inTriangle	      = filter (Triangle (xj,xi,xk) `contains`) 
			        (sublist2 xk xj inBetween)
      xl | null inTriangle    = xk     
	 | otherwise	      = minimumWith (key xi xj) inTriangle
      l			      = index xl
\end{code}



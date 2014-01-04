%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Strecken, Strahlen und Geraden (|Line|)}
\module{Line}

\begin{code}
module Line where

import Point  ( Point ((<+>),(<*>)), Point3, xcoord, ycoord, zcoord, 
		distance, sqrDistance )
import Point2 ( Point2 (..), P2, Orientation (..), orientation, angle2 )
import Maybe  ( isJust, fromJust )
import qualified Point2 (translate, rotateOrg, reflect, rotate)
\end{code}

\begin{code}
data (Point p, Num a) => Line p a 
			      = Segment { point1, point2 :: p a }
                              | Ray     { point1, point2 :: p a }
                              | Line    { point1, point2 :: p a } 

type Line2 a		      = Line Point2 a
type L2	a		      = Line Point2 a
type Line2D		      = Line2 Double
type Line3 a		      = Line Point3 a
\end{code}

\begin{code}
segmentToRay, segmentToLine, 
    rayToLine		      :: (Point p, Num a) => Line p a -> Line p a
segmentToRay (Segment x y)    = Ray x y
segmentToLine (Segment x y)   = Line x y
rayToLine (Ray x y)           = Line x y

source, target                :: (Point p, Num a) => Line p a -> p a
source (Segment a _)          = a
target (Segment _ b)          = b

mapLine                       :: (Point p, Num a, Num b) => (p a -> p b) -> Line p a -> Line p b
mapLine f (Segment x y)	      = Segment (f x) (f y)
mapLine f (Ray x y)	      = Ray (f x) (f y)
mapLine f (Line x y)	      = Line (f x) (f y)
\end{code}

\begin{code}
xcoord1, xcoord2, 
    ycoord1, ycoord2,
    zcoord1, zcoord2	      :: (Point p, Num a) => Line p a -> a
xcoord1                       = xcoord . point1
ycoord1                       = ycoord . point1
zcoord1                       = zcoord . point1
xcoord2                       = xcoord . point2
ycoord2                       = ycoord . point2
zcoord2                       = zcoord . point2
\end{code}

|dx|, |dy| sind die Differenzen der $x$- bzw. der $y$-Koordinaten der zwei Punkte.

\begin{code}
dx,dy                         :: Num a => L2 a -> a
dx s                          = xcoord2 s - xcoord1 s
dy s                          = ycoord2 s - ycoord1 s

isVertical, isHorizontal      :: Num a => L2 a -> Bool
isVertical s                  = ycoord1 s == ycoord2 s
isHorizontal s                = xcoord1 s == xcoord2 s

horizontal, vertical          :: Num a => a -> L2 a
horizontal y                  = Line (Point2 (0, y)) (Point2 (1, y))
vertical x                    = Line (Point2 (x, 0)) (Point2 (x,1))
\end{code}

Die Steigung einer Geraden wird als Datentyp |Slope| gespeichert. Ein Nachteil dieser
Darstellung ist, daß die Darstellung nicht eindeutig ist, $-0=+0$. 

\begin{code}
data Fractional a => Slope a  = Vertical | Slope a 
				deriving (Eq, Show)

slope                         :: Fractional a => L2 a -> Slope a
slope s | dx' == 0	      = Vertical 
	| otherwise	      = Slope (dy s / dx')
  where dx'                   = dx s

areParallel                   :: Fractional a => L2 a -> L2 a -> Bool
areParallel s t               = slope s == slope t
\end{code}

|direction| ist die Richtung bezüglich der $x$-Achse.
    
\begin{code}
direction                     :: RealFloat a => L2 a -> a
direction s | a/=0 || b/=0    = atan2 b a
	    | otherwise	      = 0
  where a                     = dx s
        b                     = dy s    
\end{code}

|angle| ermittelt den Winkel zwischen zwei Linien.

\begin{code}
angle			      :: Line2D -> Line2D -> Double
angle s t		      = angle2 (point2 s - point1 s) (point2 t - point1 t)
\end{code}

|translate|, |rotate| und |reflect| sind kanonische Erweiterungen der
entsprechenden Funktionen auf Punkten.

\begin{code}
translate                     :: (Floating a, Ord a) => L2 a -> a -> a -> L2 a
translate s phi d             = mapLine (\ x -> Point2.translate x phi d) s

rotate                        :: (Floating a, Ord a) => L2 a -> P2 a -> a -> L2 a
rotate s r phi                = mapLine (\ x -> Point2.rotate x r phi) s

rotateOrg                     :: (Floating a, Ord a) => L2 a -> a -> L2 a
rotateOrg s phi               = mapLine (\ x -> Point2.rotateOrg x phi) s

reflect                       :: Fractional a => L2 a -> P2 a -> P2 a -> L2 a
reflect s p q                 = mapLine (\ x -> Point2.reflect x p q) s
\end{code}

|fromPDL| erstellt die Zwei-Punkte-Form aus einem Punkt, einer Richtung und einer Länge.

\begin{code}
fromPDL                       :: (Floating a, Ord a) => (P2 a -> P2 a -> b) -> P2 a -> a -> a -> b
fromPDL c p phi d	      = c p (p + (oriented phi d))
  where oriented phi d	      = Point2.rotateOrg (Point2 (d, 0)) phi
\end{code}

%
% ORIENTIERUNG
%
\subsubsection{Orientierung}

\begin{code}
orientationOfLines            :: (Num a, Ord a) => L2 a -> L2 a -> Orientation
orientationOfLines s t
  | p == p'                   = orientation p (point2 s) (point2 t)
  | otherwise                 = error "Line2.orientationOfLines: point1 s/=point1 t"
  where p                     = point1 s
        p'                    = point1 t
\end{code}

%
% SCHNITTPUNKTE
%
\subsubsection{Schnittpunkte}

nach \cite[1.01]{cga-faq}

\begin{code}
intersect, strictIntersect    :: (Ord a, Fractional a) => L2 a -> L2 a -> Maybe (Point2 a)
doIntersect,doStrictIntersect :: (Ord a, Fractional a) => L2 a -> L2 a -> Bool

intersect s1 s2               
  | isJust res && ok s1 r && ok s2 s        = Just i
  | otherwise                               = Nothing
  where
  res                         = intersection s1 s2
  (i,r,s)                     = fromJust res
  ok (Segment _ _) r          = 0<=r && r<=1
  ok (Ray _ _) r              = r>=0
  ok (Line _ _) r             = True

{-
intersect                     = interAux paramOk
  where paramOk (Segment _ _) r = 0<=r && r<=1
        paramOk (Ray _ _) r   = r>=0
        paramOk (Line _ _) r  = True
-}

doIntersect s t               = isJust (intersect s t)

strictIntersect               = interAux paramOk
  where 
    paramOk (Segment _ _) r   = 0<r && r<1
    paramOk (Ray _ _) r       = r>0
    paramOk (Line _ _) r      = True
doStrictIntersect s t         = isJust (strictIntersect s t)

interAux 		      :: Fractional a => (Line2 a -> a -> Bool) -> Line2 a 
			      -> Line2 a -> Maybe (Point2 a)
interAux ok s1 s2             = if isJust res && ok s1 r && ok s2 s then Just i else Nothing
  where
  res                         = intersection s1 s2
  (i,r,s)                     = fromJust res
  
intersection                 :: Fractional a => L2 a -> L2 a -> Maybe (Point2 a,a,a)
intersection s1 s2
  | denom == 0	              = Nothing
  | otherwise		      = Just (i, r, s)
  where a 		      = point1 s1
	b		      = point2 s1
	c		      = point1 s2
	d		      = point2 s2
        xa		      = xcoord a
	ya		      = ycoord a
	xb		      = xcoord b
	yb		      = ycoord b
        xc		      = xcoord c
	yc		      = ycoord c
	xd		      = xcoord d
	yd		      = ycoord d
        denom		      = (xb-xa)*(yd-yc)-(yb-ya)*(xd-xc)
        r		      = ((ya-yc)*(xd-xc)-(xa-xc)*(yd-yc)) / denom
        s		      = ((ya-yc)*(xb-xa)-(xa-xc)*(yb-ya)) / denom
--        i		      = Point2 (xa + r*(xb-xa), ya + r*(yb-ya))
        i                     = a + (r <*> (b - a))
\end{code}

\cite[1.02]{cga-faq}

\begin{code}
distanceFromLine              :: (Ord a, Floating a) => L2 a -> P2 a -> a
distanceFromLine l c	      = sqrt (sqrDistanceFromLine l c)

sqrDistanceFromLine           :: (Ord a, Fractional a) => L2 a -> P2 a -> a
sqrDistanceFromLine (Line a b) c = abs (s*s*l2) 
  where (r,s,l2,_)            = distanceAux a b c
sqrDistanceFromLine (Ray a b) c
  | r >= 0                    = abs (s*s*l2)
  | r < 0                     = sqrDistance a c 
  where (r,s,l2,_)            = distanceAux a b c
sqrDistanceFromLine (Segment a b) c
  | r < 0                     = sqrDistance a c 
  | r > 1                     = sqrDistance b c 
  | 0<=r && r<=1              = abs (s*s*l2)
  where (r,s,l2,_)            = distanceAux a b c

distanceAux		      :: Fractional a => P2 a -> P2 a -> P2 a -> (a,a,a,P2 a)
distanceAux a@(Point2 (xa,ya)) b@(Point2 (xb,yb)) c@(Point2 (xc,yc))
                              = (r, s, l2, i)
  where l2                    = sqrLengthOfSegment (Segment a b)
        r                     = ((ya-yc)*(ya-yb)-(xa-xc)*(xb-xa)) / l2
        s                     = ((ya-yc)*(xb-xa)-(xa-xc)*(yb-ya)) / l2
--        i                     = Point2 (xa + r*(xb-xa), ya + r*(yb-ya))
	i		      = a + (r <*> (b - a))
\end{code}

\begin{code}
lengthOfSegment               :: (Point p, Floating a) => Line p a -> a
lengthOfSegment (Segment p q) = distance p q

sqrLengthOfSegment            :: (Point p, Num a) => Line p a -> a
sqrLengthOfSegment (Segment p q) = sqrDistance p q

centerOfSegment               :: (Point p, Fractional a) => Line p a -> p a
centerOfSegment (Segment p q) = 0.5 <*> (p <+> q)

perpendicular                 :: Fractional a => L2 a -> L2 a
perpendicular s@(Segment p q) = Line p (p - (Point2 (ycoord q - ycoord p, xcoord p - xcoord q)))
  where c                     = centerOfSegment s

bisector                      :: Fractional a => L2 a -> L2 a
bisector s@(Segment p q)      = Line c (c + (Point2 (ycoord q - ycoord p, xcoord p - xcoord q)))
  where c                     = centerOfSegment s
\end{code}

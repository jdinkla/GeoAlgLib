%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Zweidimensionale Punkte (|Point2|)}
\module{Point2}

\begin{code}
module Point2 (module PointClass, module Point2) where

import PointClass
import Basics.Utilities ( minimumBy, maximumBy, OrderRel, Rel, extremaBy, Rel3 )
\end{code}

\IndexS{Point2}
\begin{code}
newtype (Num a, Eq a) => Point2 a     = Point2 (a,a) deriving Eq

type Point2D		      = Point2 Double
type P2 a		      = Point2 a

instance (Show a, Num a, Eq a) => Show (Point2 a) where
    showsPrec _ (Point2 (x,y))  = shows (x,y)

instance Point Point2 where
    dimension _               = 2
    origin                    = Point2 (0,0)
    ith 1 (Point2 (x,_))      = x
    ith 2 (Point2 (_,y))      = y
    ith _ _                   = error "Point2: ith > 2"
    mapP f (Point2 (x,y))     = Point2 (f x, f y)

    (Point2 (x,y)) <==> (Point2 (u,v)) = x==u && y==v
    (Point2 (x,y)) </=> (Point2 (u,v)) = x/=u || y/=v
    (Point2 (x,y)) <+> (Point2 (u,v))  = Point2 (x+u,y+v)
    (Point2 (x,y)) <-> (Point2 (u,v))  = Point2 (x-u,y-v)
    (Point2 (x,y)) <.> (Point2 (u,v))  = x*u + y*v

instance (Num a, Eq a) => Num (Point2  a) where
    (+)			      = (<+>)
    (-)			      = (<->)
    negate		      = negateP
    (*)			      = undefined
    abs			      = mapP abs
    signum		      = undefined
    fromInteger		      = undefined
\end{code}

Das Kreuzprodukt für zweidimensonale Punkte stammt aus
\cite[S. 887-888]{cormen90:introduction}, (siehe auch
\cite[S. 19]{orourke94:cg}).

\begin{code}
cross2                        :: (Num a, Eq a) => Point2 a -> Point2 a -> a
x `cross2` y		      = xcoord x*ycoord y - xcoord y*ycoord x
\end{code}

\Index{area2} ermittelt die Determinante
\begin{eqnarray*}
\mathtt{area2\ p\ q\ r} = \det \left|\matrix{ p_x\ p_y\ 1 \cr q_x\ q_y\ 1 \cr r_x\ r_y\ 1} \right|
\end{eqnarray*}

\begin{code}
area2                         :: (Num a, Eq a) => Point2 a -> Point2 a -> Point2 a -> a
area2 (Point2 (px,py)) (Point2 (qx,qy)) (Point2 (rx,ry))
                              = (px-qx) * (py-ry) - (py-qy) * (px-rx)
\end{code}

%
% WINKEL
%
\subsubsection{Winkel}

\begin{code}
angle			      :: (Floating a, Ord a) => P2 a -> a
angle (Point2 (x,y))	      = psy
  where c		      = sqrt (x^2 + y^2)
        phi | c == 0	      = 0
	    | otherwise       = asin (y/c)
	psy | x < 0 	      = if y < 0 then -pi-phi else pi-phi
	    | otherwise	      = phi

angle2			     :: (Floating a, Ord a) => P2 a -> P2 a -> a
angle2 x y | denum == 0      = 0
	   | cosPhi >= 1.0   = 0
	   | cosPhi <= -1.0  = pi
	   | cross2 x y >= 0 = acos cosPhi
	   | otherwise       = - acos cosPhi
  where denum                = (x <.> x) * (y <.> y)
        cosPhi               = (x <.> y) / sqrt denum

angleWrt, angle3	      :: (Floating a, Ord a) => P2 a -> P2 a -> P2 a -> a
angleWrt p q r                = angle2 (q-p) (r-p)
angle3 p q r                  = (-1) * angle2 (p-q) (r-q)

angleEps		      :: (Floating a, Ord a) => a -> P2 a -> P2 a -> a
angleEps eps p q              = if abs (r-pi) < eps then 0 else r
  where r                     = angle2 p q
\end{code}

\Index{translate} ist die Translation des Punktes $p$ in Richtung $a$ um die
Entfernung $d$.  \Index{rotate} rotiert den Punkt $p$ um den Winkel $\varphi$
bezüglich des Punkts $q$.  \Index{rotateOrg} rotiert den Punkt $p$ um den Winkel
$\varphi$ bezüglich des Ursprungs.  \Index{reflect} spiegelt den Punkt $p$ an
der Geraden $\overline{qr}$.

\begin{code}
translate                     :: (Floating a, Ord a) => P2 a -> a -> a -> P2 a
translate (Point2 (x,y)) a d  = Point2 (x + cos a*d, y + sin a*d)

rotate                        :: (Floating a, Ord a) => P2 a -> P2 a -> a -> P2 a
rotate p o phi                = o + (rotateOrg (p-o) phi)

rotateOrg                     :: (Floating a, Ord a) => P2 a -> a -> P2 a
rotateOrg (Point2 (x,y)) phi  = Point2 (x*cos phi - y*sin phi, x*sin phi + y*cos phi)

reflect                       :: (Fractional a, Eq a) => P2 a -> P2 a -> P2 a -> P2 a
reflect p q r                 = Point2 (xcoord q + xcoord p1*cos2 - ycoord p1*sin2, 
                                        ycoord q + xcoord p1*sin2 + ycoord p1*cos2)
  where p1                    = p <-> q 
        p2                    = r <-> q
        l                     = (p1 <.> p1) * (p2 <.> p2)
        cosPhi                = p1 <.> p2
        sinPhi                = p1 `cross2` p2
        cos2                  = (cosPhi^2 - sinPhi^2)/l
        sin2                  = 2*cosPhi*sinPhi/l
\end{code}

%
% BOOLESCHE UND ORDERING-RELATIONEN.
%
\subsubsection{Boolesche und \textit{Ordering}-Relationen}

\begin{code}
lexic2			      :: (Num a, Ord a) => OrderRel (P2 a)
p `lexic2` q		      = case compareX p q of
    				  EQ -> compareY p q
	                          x  -> x
{-
(Point2 (x,y)) `lexic2` (Point2 (u,v))  
  | x < u		      = LT 
  | x == u		      = compare y v 
  | x > u		      = GT
-}

above, aboveOrOn, aboveOrRight, aboveOrLeft  :: (Num a, Ord a) => Rel (P2 a)
(Point2 (x,y)) `above` (Point2 (u,v))	     = y>v
(Point2 (x,y)) `aboveOrOn` (Point2 (u,v))    = y>=v
(Point2 (x,y)) `aboveOrRight` (Point2 (u,v)) = y>v || (y==v && x>u)
(Point2 (x,y)) `aboveOrLeft` (Point2 (u,v))  = y>v || (y==v && x<u)

below, belowOrOn, belowOrRight, belowOrLeft  :: (Num a, Ord a) => Rel (P2 a)
(Point2 (x,y)) `below` (Point2 (u,v))	     = y<v
(Point2 (x,y)) `belowOrOn` (Point2 (u,v))    = y<=v
(Point2 (x,y)) `belowOrRight` (Point2 (u,v)) = y<v || (y==v && x>u)
(Point2 (x,y)) `belowOrLeft` (Point2 (u,v))  = y<v || (y==v && x<u)

leftOf, leftOfOrOn, leftOrBelow, leftOrAbove :: (Num a, Ord a) => Rel (P2 a)
(Point2 (x,y)) `leftOf` (Point2 (u,v))	     = x<u
(Point2 (x,y)) `leftOfOrOn` (Point2 (u,v))   = x<=u
(Point2 (x,y)) `leftOrBelow` (Point2 (u,v))  = x<u || (x==u && y<v) 
(Point2 (x,y)) `leftOrAbove` (Point2 (u,v))  = x<u || (x==u && y>v)

rightOf, rightOfOrOn, rightOrBelow, rightOrAbove :: (Num a, Ord a) => Rel (P2 a)
(Point2 (x,y)) `rightOf` (Point2 (u,v))	      = x>u
(Point2 (x,y)) `rightOfOrOn` (Point2 (u,v))   = x>=u
(Point2 (x,y)) `rightOrBelow` (Point2 (u,v))  = x>u || (x==u && y<v)
(Point2 (x,y)) `rightOrAbove`  (Point2 (u,v)) = x>u || (x==u && y>v)
\end{code}

\begin{code}
leftestLowest, rightestLowest, leftestHighest, rightestHighest,
  lowestLeftest, highestLeftest, lowestRightest, highestRightest,
  lowest, highest, rightest, leftest :: (Num a, Ord a) => [Point2 a] -> Point2 a

leftestLowest                 = minimumBy belowOrLeft
rightestHighest               = maximumBy belowOrLeft
rightestLowest                = minimumBy belowOrRight
leftestHighest                = maximumBy belowOrRight
lowestLeftest                 = minimumBy leftOrBelow
highestRightest               = maximumBy leftOrBelow
highestLeftest                = minimumBy leftOrAbove
lowestRightest                = maximumBy leftOrAbove
highest                       = leftestLowest
lowest                        = leftestHighest
leftest                       = lowestLeftest
rightest                      = lowestRightest
\end{code}

%
% ORIENTIERUNG
%
\subsubsection{Orientierung}

\Index{orientation} ermittelt die Orientierung des Punktes \texttt{r} bezüglich
der Strecke $\overline{\mathtt{pq}}$ \cite[S.30-31]{orourke94:cg}.
\IndexS{Orientation}\IndexS{orientationOfLines}

\begin{code}
data Orientation              = Collinear | Clockwise | CounterClockwise 
                              deriving (Eq, Show)
                 
orientation                   :: (Num a, Ord a) => Point2 a -> Point2 a -> Point2 a -> Orientation
orientation p q r
  | s>0                       = CounterClockwise -- to the left
  | s==0                      = Collinear
  | s<0                       = Clockwise        -- to the right
  where s                     = area2 p q r

caseOrientation		      :: (Ord a, Num a) => Bool -> Bool -> Bool -> Rel3 (Point2 a)
caseOrientation a b c p q r   = case orientation p q r of
				  CounterClockwise -> a
				  Collinear -> b
				  Clockwise -> c
\end{code}

Anm. Der kollineare Fall muß für Fließkommazahlen mit $\varepsilon$-Umgebungen
implementiert werden. \cite[S.30-31]{orourke94:cg}.

\begin{code}
isCollinear, isRightTurn, isLeftTurn, isLeftTurnOrOn, 
  isRightTurnOrOn, isOn, isLeftTurnOrNearer, isRightTurnOrNearer
		              :: (Num a, Ord a) => Rel3 (Point2 a)
isLeftTurn                    = caseOrientation True   False  False
isLeftTurnOrOn                = caseOrientation True   True   False
isLeftTurnOrNearer s p q      = caseOrientation True (compareDistance s p q) False s p q
isCollinear                   = caseOrientation False  True   False
isRightTurnOrOn		      = caseOrientation False  True   True
isRightTurnOrNearer s p q     = caseOrientation False (compareDistance s p q) True s p q
isRightTurn                   = caseOrientation False  False  True
isOn p q r                    = orientation p q r == orientation q p r

compareDistance s p q         = sqrDistance s p <= sqrDistance s q
\end{code}

\begin{code}
clockwise3, cclockwise3       :: (Ord a, Num a) => [Point2 a] -> [Point2 a]
clockwise3 []		      = []
clockwise3 [x]		      = [x]
clockwise3 [x,y]	      = [x,y]
clockwise3 xs@[x,y,z]         = case orientation x y z of
                                  CounterClockwise -> [x,z,y]
				  Collinear -> extremaBy leftOrBelow xs
				  Clockwise -> xs
clockwise3 _		      = error "clockwise3 for more than three points"

cclockwise3 []		      = []
cclockwise3 [x]		      = [x]
cclockwise3 [x,y]	      = [x,y]
cclockwise3 xs@[x,y,z]        = case orientation x y z of
                                  CounterClockwise -> xs
				  Collinear -> extremaBy leftOrBelow xs
				  Clockwise -> [x,z,y]
cclockwise3 _		      = error "cclockwise3 for more than three points"
\end{code}


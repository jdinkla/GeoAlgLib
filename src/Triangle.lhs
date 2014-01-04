%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Dreiecke (|Triangle|)}
\module{Triangle}

\begin{code}
module Triangle where

import Point  ( Point, Point3 )
import Point  ( Point2, area2, orientation, Orientation (Collinear) )
import Line   ( Line (Segment) )
import Circle ( circleFrom3Points )
import qualified Circle as C (Circle (center))
import Data.Maybe  ( fromJust )
import Basics.Utilities ( Rel3 )
\end{code}

Ein Dreieck \Index{Triangle} ist ein Tripel von Punkten.

\begin{code}
newtype (Point p, Num a)      => Triangle p a   
                              = Triangle (p a, p a, p a)

type Triangle2 a	      = Triangle Point2 a
type Triangle3 a	      = Triangle Point3 a

vertices		      :: (Num a, Point p) => Triangle p a -> [p a]
vertices (Triangle (p,q,r))   = [p,q,r]

segments		      :: (Num a, Point p, Eq a) => Triangle p a -> [Line p a]
segments (Triangle (p,q,r))   = [Segment p q, Segment q r, Segment r p]

area			      :: (Fractional a, Eq a) => Triangle Point2 a -> a
area (Triangle (p,q,r))       = 0.5 * area2 p q r

contains, containsBNV	      :: (Num a, Ord a, Eq a) => Triangle Point2 a -> Point2 a -> Bool
contains tri@(Triangle (s,t,v)) p = containsBNV tri p || p==s || p==t || p==v

containsBNV tri p             = checkBy test tri p
  where test Collinear b c    = b==c
        test a Collinear c    = a==c
        test a b Collinear    = a==b
        test a b c            = a==b && b==c

inInteriourOf		      :: (Num a, Ord a) => Point2 a -> Triangle Point2 a -> Bool
inInteriourOf p tri           = checkBy test tri p
  where test Collinear b c    = False
        test a Collinear c    = False
        test a b Collinear    = False
        test a b c            = a==b && b==c

checkBy :: (Num a, Ord a) => Rel3 Orientation -> Triangle2 a -> Point2 a -> Bool
checkBy chk (Triangle (s,t,v)) p = chk a b c
  where a                     = orientation s t p
        b                     = orientation t v p
        c                     = orientation v s p
\end{code}

Manche Algorithmen benutzen nur das Vorzeichen des Flächeninhalts, um die Orientierung der
Punkte bestimmen. Dieses kann mit der Funktion |orientation| aus dem Modul |Lines| geschehen.

|center| ermittelt den Mittelpunkt des Umkreises eines Dreiecks.

\begin{code}
center			      :: (Fractional a, Eq a) => Triangle2 a -> Point2 a
center (Triangle (p, q, r))   = C.center (fromJust (circleFrom3Points p q r))
\end{code}

%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsubsection{Eindimensionale Punkte}
\module{Point1}

\begin{code}
module Point1 (module PointClass, module Point1) where

import PointClass
\end{code}

Auch eindimensionale Punkte werden definiert, damit zum Beispiel eindimensionale $k$-d Bäume
benutzt werden können.

\begin{code}
newtype Num a => Point1 a     = Point1 a deriving (Eq, Ord)

type P1 a		      = Point1 a

instance (Num a, Show a)      => Show (Point1 a) where
    showsPrec _ (Point1 x)    = shows x

instance Point Point1 where
    dimension _		      = 1
    origin		      = Point1 0
    ith 1 (Point1 x)	      = x
    ith _ _		      = error "Point1: ith > 1"
    mapP f (Point1 x)	      = Point1 (f x)
    
    (Point1 x) <==> (Point1 y) = x == y
    (Point1 x) <+> (Point1 y) = Point1 (x+y)
    (Point1 x) <-> (Point1 y) = Point1 (x-y)
    (Point1 x) <.> (Point1 y) = x*y

instance (Num a, Eq a)        => Num (Point1 a) where
    (+)			      = (<+>)
    (-)			      = (<->)
    negate		      = negateP
    (Point1 x) * (Point1 y)   = Point1 (x*y)
    abs	(Point1 x)	      = Point1 (abs x)
    signum		      = undefined
    fromInteger 	      = Point1 . fromInteger
\end{code}


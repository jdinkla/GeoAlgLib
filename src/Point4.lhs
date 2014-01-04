%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Vierdimensionale Punkte (|Point4|)}
\module{Point4}

\begin{code}
module Point4 (module PointClass, module Point4) where

import PointClass
\end{code}

Vierdimensionale Punkte \Index{Point4} sind noch als Tupel implementiert.

\begin{code}
newtype Num a => Point4 a     = Point4 (a,a,a,a) deriving Eq

type P4	a		      = Point4 a

instance (Num a, Show a) => Show (Point4 a) where
    showsPrec _ (Point4 (x,y,z,u)) = shows (x,y,z,u)

instance Point Point4 where
    origin                    = Point4 (0,0,0,0)
    dimension _               = 4
    ith 1 (Point4 (x,_,_,_))  = x
    ith 2 (Point4 (_,y,_,_))  = y
    ith 3 (Point4 (_,_,z,_))  = z
    ith 4 (Point4 (_,_,_,u))  = u
    ith _ _                   = error "Point4: ith > 4"
    mapP f (Point4 (x,y,z,u)) = Point4 (f x, f y, f z, f u)

    (Point4 (x,y,z,u)) <==> (Point4 (x',y',z',u')) 
                              = x==x' && y==y'&& z==z' && u==u'
    (Point4 (x,y,z,u)) <+> (Point4 (x',y',z',u')) 
                              = Point4 (x+x',y+y',z+z',u+u')
    (Point4 (x,y,z,u)) <-> (Point4 (x',y',z',u')) 
                              = Point4 (x-x',y-y',z-z',u-u')
    (Point4 (x,y,z,u)) <.> (Point4 (x',y',z',u')) 
                              = x*x' + y*y' + z*z' + u*u'

instance (Num a, Eq a)        => Num (Point4 a) where
    (+)			      = (<+>)
    (-)			      = (<->)
    negate		      = negateP
    (*)			      = undefined
    abs			      = mapP abs
    signum		      = undefined
    fromInteger		      = undefined
\end{code}


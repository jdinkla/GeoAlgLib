%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Dreidimensionale Punkte (|Point3|)}
\module{Point3}

\begin{code}
module Point3 (module PointClass, module Point3) where

import PointClass
\end{code}

\begin{code}
newtype (Num a, Eq a) => Point3 a     = Point3 (a,a,a) deriving Eq
type P3 a		      = Point3 a

instance (Num a, Show a, Eq a) => Show (Point3 a) where
    showsPrec _ (Point3 (x,y,z)) = shows (x,y,z)

instance Point Point3 where
    origin                    = Point3 (0,0,0)
    dimension _               = 3
    ith 1 (Point3 (x,_,_))    = x
    ith 2 (Point3 (_,y,_))    = y
    ith 3 (Point3 (_,_,z))    = z
    ith _ _                   = error "Point3: ith > 3"
    mapP f (Point3 (x,y,z))   = Point3 (f x, f y, f z)

    (Point3 (x,y,z)) <==> (Point3 (x',y',z')) 
                              = x==x' && y==y'&& z==z'
    (Point3 (x,y,z)) <+> (Point3 (x',y',z')) 
                              = Point3 (x+x',y+y',z+z')
    (Point3 (x,y,z)) <-> (Point3 (x',y',z')) 
                              = Point3 (x-x',y-y',z-z')
    (Point3 (x,y,z)) <.> (Point3 (x',y',z')) 
                              = x*x' + y*y' + z*z'

instance (Num a, Eq a)        => Num (Point3 a) where
    (+)			      = (<+>)
    (-)			      = (<->)
    negate		      = negateP
    (*)			      = undefined
    abs			      = mapP abs
    signum		      = undefined
    fromInteger		      = undefined

cross3                        :: (Num a, Eq a) => Point3 a -> Point3 a -> Point3 a
(Point3 (x1,x2,x3)) `cross3` (Point3 (y1,y2,y3)) 
                              = Point3 (x2*y3 - x3*y2, x3*y1 - x1*y3, x1*y2 - x2*y1)
\end{code}

nach \cite[S. 26-27, 142]{orourke94:cg}. Zum Problem des Overflows
siehe Seite 157ff, ebd.

\begin{code}
volume6	:: (Num a, Eq a) => P3 a -> P3 a -> P3 a -> P3 a -> a
volume6 (Point3 (a0,a1,a2)) (Point3 (b0,b1,b2)) 
        (Point3 (c0,c1,c2)) (Point3 (d0,d1,d2))
  = -b0*c1*d2 + a0*c1*d2 + b1*c0*d2 - a1*c0*d2 - a0*b1*d2
    +a1*b0*d2 + b0*c2*d1 - a0*c2*d1 - b2*c0*d1 + a2*c0*d1
    +a0*b2*d1 - a2*b0*d1 - b1*c2*d0 + a1*c2*d0 + b2*c1*d0
    -a2*c1*d0 - a1*b2*d0 + a2*b1*d0 + a0*b1*c2 - a1*b0*c2
    -a0*b2*c1 + a2*b0*c1 + a1*b2*c0 - a2*b1*c0
\end{code}

%
%volume6 (Point3 (512,512,512)) (Point3 (512,-512,-512)) (Point3 (-512,512,-512)) (Point3 (-512,-512,512)) :: Integer
%2147483648 :: Integer
%
%volume6 (Point3 (600003,0,0)) (Point3 (0,600003,0)) (Point3 (0,0,600003)) (Point3 (200001,200001,200001))
%0 :: Int
%
% !!! *** ABER *** !!!
%
%volume6 (Point3 (600003,0,0)) (Point3 (0,600003,0)) (Point3 (0,0,600003)) (Point3 (200001,200001,200001)) :: Double
%-1.71799e+10 :: Double
%

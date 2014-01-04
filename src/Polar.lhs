%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Polarkoordinaten (\texttt{Polar})}
\module{Polar}

\begin{code}
module Polar (
         Polar (..), fromPolar, toPolar,  
         leq, leqInv, geq, geqInv, equalAngle, leqLength, 
	 similiarAngle
       ) where

import Point2	 ( Point2 (Point2) )
\end{code}

\begin{code}
data Polar                    = Polar { len, angle :: Double } deriving (Eq, Show)

fromPolar                     :: Polar -> Point2 Double
fromPolar (Polar r phi)       = Point2 (r*cos phi,r*sin phi)

toPolar                       :: Point2 Double -> Polar
toPolar (Point2 (x,y))        = Polar c psy
  where c		      = sqrt (x^2 + y^2)
        phi | x == 0          = case compare y 0 of
				  LT -> -pi/2; EQ -> 0; GT -> pi/2
	    | otherwise       = atan (y/x)
	psy | x < 0 	      = if y < 0 then -pi+phi else pi+phi
	    | otherwise	      = phi
\end{code}

Folgende Ordnungen sind auf Polarkoordinaten definiert

\begin{code}
leq, leqInv, geq, geqInv, equalAngle, leqLength :: Polar -> Polar -> Bool
(Polar x phi) `leq` (Polar y psi)     = phi<psi || (phi==psi && x<=y)
(Polar x phi) `leqInv` (Polar y psi)  = phi<psi || (phi==psi && y<=x)
(Polar x phi) `geq` (Polar y psi)     = phi>psi || (phi==psi && x<=y)
(Polar x phi) `geqInv` (Polar y psi)  = phi>psi || (phi==psi && x>=y)
(Polar _ phi) `equalAngle` (Polar _ psi) = phi == psi
(Polar x _) `leqLength` (Polar y _)   = x<=y

similiarAngle :: Double -> Polar -> Polar -> Bool
similiarAngle eps (Polar _ x) (Polar _ y) = abs (x - y) < eps
\end{code}



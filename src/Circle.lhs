%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Kreise (|Circle|)}
\module{Circle}

\begin{code}
module Circle where

import Point (Point, Point2 (..), P2, Point3 )
\end{code}

Ein Kreis |Circle| wird durch seinen Mittelpunkt und dem Quadrat seines Radius angegeben.

\begin{code}
data (Point p, Num a)         => Circle p a
			      = Circle { center :: p a, radius :: a }

type Circle2 a		      = Circle Point2 a
type Circle3 a		      = Circle Point3 a
\end{code}

nach \cite[1.04]{cga-faq}

\begin{code}
circleFrom3Points             :: (Fractional a, Eq a) => P2 a -> P2 a -> P2 a -> Maybe (Circle2 a)
circleFrom3Points (Point2 (ax,ay)) (Point2 (bx,by)) (Point2 (cx,cy))
  | g==0                      = Nothing
  | otherwise                 = Just (Circle (Point2 (px, py)) r2)
  where a 		      = bx - ax	-- b+1
	b		      = by - ay
	c		      = cx - ax
	d		      = cy - ay
	e		      = a*(ax + bx) + b*(ay + by) -- 2b+2
	f		      = c*(ax + cx) + d*(ay + cy)
	g		      = 2.0*(a*(cy - by)-b*(cx - bx)) -- 2b+3
	px		      = (d*e - b*f) / g	-- 2b+3 / 2b+3 = ?
	py		      = (a*f - c*e) / g
	r2		      = (ax - px)^2 + (ay - py)^2 -- 2b+1
\end{code}



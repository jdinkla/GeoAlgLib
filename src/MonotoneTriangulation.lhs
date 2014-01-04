%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Triangulierung monotoner Polygone (|MonotoneTriangulation|)}
\module{MonotoneTriangulation}

\cite[K. 2.1]{orourke94:cg}\cite[K. 3.3]{berg97:cg}

\begin{code}
module MonotoneTriangulation ( 
         sort, monoTri
       )
where

import Point2    ( Point2, P2, aboveOrLeft, isLeftTurn, isRightTurn, 
		   leftestHighest, rightestLowest )
import Polygon   ( Polygon2, vertices )
import Triangle  ( Triangle (..), Triangle2 )
import Basics.Sorting   ( mergeBy )
import Basics.Utilities ( longerThan, sublist, split )
\end{code}

\begin{code}
type Pt a                     = Either (P2 a) (P2 a)

value                         :: Pt a -> (P2 a)
value (Left x)		      = x
value (Right x)		      = x

areOpposite		      :: Pt a -> Pt a -> Bool
areOpposite (Right _) (Right _) = False
areOpposite (Left _) (Left _)   = False
areOpposite _ _		        = True
\end{code}

Mit |sort| wird ein Polygon nach $y$-Koordinaten sortiert und jeder Knoten der linken oder
rechten Kette zugewiesen.

\begin{code}
sort                          :: (Ord a, Num a) => [P2 a] -> [Pt a]
sort                          = mergeChains . splitIntoChains

mergeChains                   :: (Ord a, Num a) => ([Pt a], [Pt a]) -> [Pt a]
mergeChains (ls, rs)          = mergeBy cmp ls rs
  where cmp x y               = value x `aboveOrLeft` value y

splitIntoChains               :: (Ord a, Num a) => [P2 a] -> ([Pt a], [Pt a])
splitIntoChains ps            = (map Left ls, map Right (reverse rs))
  where ls		      = sublist hi lo ps
        rs                    = tail (init (sublist lo hi ps))
	lo		      = rightestLowest ps
	hi		      = leftestHighest ps
\end{code}

nach \cite{berg97:cg}

\begin{code}
monoTri                       :: (Ord a, Num a) => Polygon2 a -> [Triangle2 a]
monoTri p                     = triangulate [v,u] vs
  where (u:v:vs)	      = sort (vertices p)

triangulate		      :: (Ord a, Num a) => [Pt a] -> [Pt a] -> [Triangle2 a]
triangulate _ []	      = []
triangulate [s] (v:vs)	      = triangulate [v,s] vs
triangulate st@(s:_:_) (v:vs)  
  | areOpposite v s           = popAll ++ triangulate [v,s] vs
  | otherwise		      = popSome v st 
  where popAll	     	      = [triangle v x y | (x,y) <- zip st (tail st)]
	popSome v [s]         = triangulate [v,s] vs
	popSome v st@(w:s:ss)
	  | isConvex v w s    = triangle v w s : popSome v (s:ss)
	  | otherwise	      = triangulate (v:st) vs

isConvex		      :: (Ord a, Num a) => Pt a -> Pt a -> Pt a -> Bool
isConvex (Left x) y z	      = isRightTurn x (value y) (value z)
isConvex (Right x) y z	      = isLeftTurn  x (value y) (value z)

triangle                      :: Num a => Pt a -> Pt a -> Pt a -> Triangle2 a
triangle x y z                = Triangle (value x, value y, value z)
\end{code}


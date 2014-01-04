%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Ein einfacher Algorithmus (|KETTriangulation|)}
\module{KETTriangulation}

nach \cite{toussaint91:simplepolygons}

\begin{code}
module KETTriangulation (
         ketTri
       )
where 

import Point     ( P2, isRightTurnOrOn, isLeftTurn )
import Polygon   ( Polygon2, angles, vertices )
import Triangle  ( Triangle (..), Triangle2, containsBNV )
import Data.List      ( (\\) )

ketTri                        :: (Ord a, Num a) => Polygon2 a -> [Triangle2 a]
ketTri poly		      = scan vs stack rs
  where ps@(p1:p2:p3:qs)      = vertices poly
        vs                    = qs ++ [p1]
        stack                 = [p3, p2, p1, last ps]
        rs                    = reflexVertices ps

scan	                      :: (Ord a, Num a) => [P2 a] -> [P2 a] -> [P2 a] 
			      -> [Triangle2 a]
scan [v] [x_p, x_i, _, _] rs  = [Triangle (x_i, x_p, v)]
scan (v:vs) ss@[_,_,_] rs     = scan vs (v:ss) rs 
scan vs@(v:vs') ss@(x_p:x_i:ss'@(x_m:x_mm:xs)) rs   
  | isEar rs x_m x_i x_p      = Triangle (x_m, x_i, x_p) : scan vs (x_p:ss') rs'
  | otherwise                 = scan vs' (v:ss) rs
  where rs'		      = rs \\ (isConvex x_m x_p v ++ isConvex x_mm x_m x_p)
	isConvex im i ip      = if isLeftTurn im i ip then [i] else []

isEar                         :: (Ord a, Num a) => [P2 a] -> P2 a -> P2 a -> P2 a -> Bool
isEar [] _ _ _                = True 
isEar rs m x p		      = isLeftTurn m x p
                                && not (any ((Triangle (m,x,p)) `containsBNV`) rs)

reflexVertices                :: (Ord a, Num a) => [P2 a] -> [P2 a]
reflexVertices xs             = [ x | (m,x,p) <- angles xs, isRightTurnOrOn m x p ]
\end{code}




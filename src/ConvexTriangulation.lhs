%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Triangulierung konvexer Polygone (|ConvexTriangulation|)}
\module{ConvexTriangulation}

> module ConvexTriangulation ( 
>          convexTri 
>        )
> where
> 
> import Triangle		  ( Triangle (Triangle), Triangle2 )
> import Polygon		  ( Polygon2, vertices )
> 
> convexTri			  :: Num a => Polygon2 a -> [Triangle2 a]
> convexTri poly		  = [Triangle (p,x,y) | (x,y) <- zip ps (tail ps)]
>   where (p:ps)		  = vertices poly

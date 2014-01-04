%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Triangulation einer Punktmenge (|MergeTriangulation|)}
\module{MergeTriangulation}

> module MergeTriangulation ( mergeTri, mergeTriX ) 
> where
> 
> import Point2    ( P2, Point2, clockwise3, below, 
>		     leftest, rightest, equalX, lexic2, compareX )
> import Triangle  ( Triangle (..), Triangle2 )
> import Polygon   ( Polygon (..) )
> import Divide    ( SplitTree (..), splitTree, reduce )
> import MergeHull ( lowerBridge, upperBridge )
> import AdaptTriangulation ( adaptTri )
> import Basics.Utilities ( split, extremaBy, sublist )

Eine schöne Anwendung von |adaptTri|: Triangulation einer Punktmenge
in optimalen $O(n\log n)$.

> mergeTri                      :: (Ord a, Floating a) => [P2 a] -> [Triangle2 a]
> mergeTri                      = fst . mergeTriX
> 
> mergeTriX                     :: (Ord a, Floating a) 
>                               => [P2 a] -> ([Triangle2 a], [P2 a])
> mergeTriX                     = reduce f merge
>                               . splitTree lexic2 compareX
>   where f [x]                 = ([], [x])
>	  f _			= error "mergeTri: collinear points"
> 
> merge ([], [x]) ([], [y])     = ([], [x,y])
> merge x y		        = (ls ++ rs ++ ts, ch)
>   where 
>   (ls, chL)                   = check x
>   (rs, chR)			= check y
>   (uL, uR)                    = upperBridge chL chR
>   (lL, lR)                    = lowerBridge chL chR
>   ch                          = outerR ++ outerL
>   ts                          = adaptTri (PolygonCCW inner)
>     where inner               = innerR ++ remove1 (remove2 innerL)
>           remove1             = if uR==uL then tail else id
>           remove2             = if lR==lL then init else id
>   innerL                      = sublist uL lL chL
>   outerL                      = sublist lL uL chL
>   innerR                      = sublist lR uR chR
>   outerR                      = sublist uR lR chR
>
>   check ([], xs)              = (case ys of
>                                    [x,y,z] -> [Triangle (x,y,z)]
>                                    _ -> [], ys)
>     where ys                  = clockwise3 xs
>   check x			= x



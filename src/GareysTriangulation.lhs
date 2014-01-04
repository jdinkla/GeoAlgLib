%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Der Standard-Algorithmus (|GareysTriangulation|)}
\module{GareysTriangulation}

> module GareysTriangulation ( garey ) where
> 
> import Polygon               ( Polygon2 )
> import Triangle              ( Triangle2 )
> import MonotoneTriangulation ( monoTri )
> import MonotonePartition     ( monotonePartition )

|garey| trianguliert ein einfaches Polygon in $O(n\log n)$ nach dem Algorithmus von Garey,
Johnson, Preparata und Tarjan.

> garey                         :: (Ord a, Fractional a) => Polygon2 a -> [Triangle2 a]
> garey                         = concatMap monoTri . monotonePartition


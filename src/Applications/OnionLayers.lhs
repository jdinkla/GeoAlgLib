%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Onion Layers (\texttt{OnionLayers})}
\module{OnionLayers}

\textbf{wird in der Arbeit nicht behandelt}, nur so als Beispiel implementiert.
siehe or94. Wichtig ist, das die Funktion zur Berechnung der konvexen
Hülle die kollinearen Punkte ausgibt.

> module Applications.OnionLayers ( onionLayers )
> where
> 
> import Point2    ( P2, lexic2 )
> import Polygon   ( Polygon, Polygon2, vertices )
> import GrahamsScan ( graham4 )
> import Basics.Sorting   ( nubSortBy )
> import Data.List	 ( (\\) )
> import MetaPost
> import Tests.Test
>
> 
> onionLayers		      :: (Ord a, Num a) => ([P2 a] -> Polygon2 a) -> [P2 a] -> [Polygon2 a]
> onionLayers f []	      = []
> onionLayers f ps	      = p : onionLayers f (qs \\ vs)
>   where qs              = nubSortBy lexic2 ps
>         p               = f qs
>         vs              = vertices p
>
> draw ps sc
>   = do putStrLn (pen 0.5)
>        sequence (map (\ (i,xs) -> putMP [Scaled sc, Filled, Color (i,i,i)] xs) qs)
>        putStrLn (pen 1)
>        putMP [Scaled sc] ps
>   where
>   os = onionLayers graham4 ps
>   qs = zip [0.95,0.95 - 0.5 / fromIntegral (length os)..] os
>


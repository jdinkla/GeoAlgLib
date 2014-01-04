%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Assoziationen von Punkten und Daten}
\module{Assocs}

Mit Hilfe von |Assoc| können in kd- und Bereichsbäumen beliebige Daten
mit einem Punkt assoziert werden. Zum Beispiel mit

\begin{pcode}
RangeTree.fromList [ Assoc (Point2 (3,4)) "Tankstelle", Assoc (Point2 (5,6)) "Pommes-Bude"]
\end{pcode}

> 
> module Assocs (Assoc (..), assoc) where
>
> import Point (Point (..))
>
> data Assoc d p a	      = Assoc (p a) d
> 
> assoc (Assoc p d)	      = d		      
> 
> instance Point p => Point (Assoc d p) where
>   dimension (Assoc p _)       = dimension p
>   ith i (Assoc p _)	      = ith i p
>

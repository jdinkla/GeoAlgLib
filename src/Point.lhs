%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Punkte (|Point|)}
\module{Point}

\begin{code}
module Point (
         module PointClass, module Point1, module Point2, 
         module Point3, module Point4, module PointN, module Point
       ) where

import PointClass
import Point1
import Point2
import Point3
import Point4
import PointN
\end{code}


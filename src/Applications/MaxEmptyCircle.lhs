%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Der größte leere Kreis (|MaxEmptyCircle|)}
\module{MaxEmptyCircle}

siehe \cite[K 5.3]{klein97:cg},
\cite[K. 5.5]{orourke94:cg}.

\begin{code}
module Applications.MaxEmptyCircle ( 
         maxEmptyCircle 
       ) where

import Voronoi     ( voronoi, voronoiEdge, Edges (..) )
import QEDSbasics  ( Edge (..), Orientation (..), Direction (..), EdgeRef )
import QEDSstatic  ( connected, onext, rot, ring, rprev, nodes )
import qualified   QEDSstatic as S
import Applications.AllNearest  ( minSqrDistances )
import Point2	   ( P2, sqrDistance, isLeftTurnOrOn )
import Line	   ( Line (..), Line2, intersect )
import Polygon	   ( Polygon (..), Polygon2, edges, vertices, angles )
import Circle	   ( Circle (..), Circle2 )
import Basics.Topped	   ( Topped (..), fromTopped, isFinite )
import Basics.Utilities   ( relToFst, maximumBy, uncurry3 )
import Maybe	   ( mapMaybe )
\end{code}

|maxEmptyCircle| ermittelt den größten leeren Kreise innerhalb der konvexen Hülle einer
Punktmenge.

\begin{code}
rotOrg                        :: S.QEDS (Edges a) -> EdgeRef -> (Topped a)
rotOrg q x@(i, r, _)          = f r (S.getAttr q x)
  where f Rot1                = right
        f Rot3                = left

org, dest                     :: S.QEDS (Edges a) -> EdgeRef -> a
org q x@(i, r, _)             = f r (S.getAttr q x)
  where f Rot0                = source
        f Rot2                = target
dest q x@(i, r, _)            = f r (S.getAttr q x)
  where f Rot2                = source
        f Rot0                = target

maxEmptyCircle		      :: (Ord a, Fractional a) => [P2 a] -> Circle2 a
maxEmptyCircle xs	      = Circle c r
  where
  (q, l, _)		      = voronoi xs
  hull			      = PolygonCCW (map (org q) (ring q rprev l))
  (r, c)		      = maximumBy (relToFst (<)) (minNodes++minEdges)

  minNodes		      = map (\ (r,(c,_)) -> (r,c)) 
			      $ minSqrDistances 
			        [ (fromTopped x, xs) | (x, xs) <- adjacent q,
						       isFinite x, 
						       fromTopped x `inConvex` hull ]

  adjacent                    :: S.QEDS (Edges a) -> [(Topped a,[a])]
  adjacent q                  = map mkPair (nodes Rot1 q)
    where mkPair e	      = (rotOrg q e, map (dest q . rot) (ring q onext e))

  minEdges		      = mapMaybe (mk . attributes . snd) (S.connected q)
    where mk e		      = case voronoiEdge e of
				  Nothing -> Nothing
				  Just s -> case s `intersectsPoly` hull of
					      Nothing -> Nothing
					      Just i -> Just (pair i)
            where pair i      = (sqrDistance i (source e), i)
\end{code}

|inConvex| und |intersectsPoly| sind nur naiv implementiert.

\begin{code}
inConvex		      :: (Ord a, Fractional a) => P2 a -> Polygon2 a -> Bool
p `inConvex` poly            = all (uncurry3 isLeftTurnOrOn) 
                              $ angles (vertices poly)

intersectsPoly		      :: (Fractional a, Ord a) => Line2 a -> Polygon2 a -> Maybe (P2 a)
s `intersectsPoly` poly	      = conc $ map (intersect s)
			      $ map (uncurry Segment) (edges (vertices poly))
			
conc [] = Nothing			      
conc (x:xs)	= x	      
\end{code}


%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Anwendung: Das Problem des nächsten Postamts (|NearestPoint|)}
\module{NearestPoint}

siehe \cite[K 5.3]{klein97:cg},
\cite[K. 5.5]{orourke94:cg}.

\begin{code}
module Applications.NearestPoint where

import DelaunayDAG
import Point2 (P2, sqrDistance)
import qualified Triangle as T
import Basics.Utilities (minimumWith, relToSnd)
import Control.Monad.ST ( ST )
import Control.Monad.ST (runST)
import Basics.STUtils
import Array 
import Maybe (fromJust)
\end{code}

\begin{code}
nearestPoint		      :: (Ord a, Num a) => StaticDDAG a -> P2 a -> P2 a
nearestPoint dag p
  | null vs		      = p
  | otherwise		      = minimumWith (sqrDistance p) vs
  where vs		      = concat [ vertices (simplex (getThe dag k)) 
				         | k <- locate dag p ]

--inTriangle		      :: (Ord a, Num a) => P2 a -> Simplex a -> Bool
--inTriangle x (Triangle _ _ p q r) = T.contains (T.Triangle (p,q,r)) x

type Index		      = Int

locate :: (Num a, Ord a) => StaticDDAG a -> P2 a -> [Index]
locate dag p 
  = runST (do let s = snd (bounds dag)
	      ss <- mkEmpty (1,s)
	      find ss [1,2,3,4])
  where
  find ss [] = return []
  find ss (i:is)
    = do let e_i = getThe dag i
         visited <- contains ss i
         if not visited && p `isInConflict` (simplex e_i)
            then do include ss i
                    ks <- find ss (stepsons e_i ++ sons e_i ++ is)
                    return (if not (dead e_i) then (i:ks) else ks)
            else find ss is

getThe                        :: StaticDDAG a -> Index -> Node a
getThe dag i		      = fromJust (dag ! i)

manyNearest		      :: (Ord a, Fractional a) => [P2 a] -> [P2 a] -> [(P2 a, P2 a)]
manyNearest ps qs	      = [ (p, nearestPoint dag p) | p <- qs]
  where dag		      = delaunay ps
\end{code}

%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% jdi 15.06.1997, 16.06.1997, 12.07.1997 (war sehr warm heute)
%
\subsection{Berechnung der Voronoi-Knoten (\texttt{Voronoi})}
\module{Voronoi}

\begin{code}
module Voronoi ( 
         voronoi, voronoiST, voronoiEdges, voronoiEdge,
	 addVoronoiNodes, 
	 Edges (..), org, dest 
       ) where

import QEDSbasics
import qualified QEDS as Q
import qualified QEDSstatic as S
import QEDS     ( lnext, rnext, lprev, rprev, ring )
import Delaunay ( delaunayST, DelEdge (DelEdge) )
import Point2   ( P2, Point2 (..), isLeftTurn )
import Line     ( Line (Segment, Ray), Line2, intersect, centerOfSegment, bisector )
import Basics.Topped   ( Topped (..))
import Control.Monad.ST ( ST )
import Control.Monad.ST (runST)
import Data.Maybe    ( catMaybes )
import Prelude  hiding (flip)
import Basics.Utilities ( foreach, foreach_ ) 
\end{code}

\begin{code}
data Edges a = Edges { source, target :: a, right, left :: Topped a }
	       deriving Show

org, dest                     :: Q.QEDS s (Edges a) -> EdgeRef -> ST s a
org q x@(i, r, _)             = do e <- Q.getAttr q x; return (f r e)
  where f Rot0                = source
        f Rot2                = target
dest q x@(i, r, _)            = do e <- Q.getAttr q x; return (f r e)
  where f Rot2                = source
        f Rot0                = target
\end{code}

\begin{code}
voronoi :: (Fractional a, Ord a) => [P2 a] -> (S.QEDS (Edges (P2 a)), EdgeRef, EdgeRef)
voronoi ps = runST (do (q,l,r) <- voronoiST ps
		       q' <- Q.freeze q; return (q',l,r))

voronoiST :: (Ord a, Fractional a) 
          => [P2 a] -> ST s (Q.QEDS s (Edges (P2 a)), EdgeRef, EdgeRef)
voronoiST ps = delaunayST ps >>= addVoronoiNodes

addVoronoiNodes :: (Ord a, Fractional a) 
		=> (Q.QEDS s (DelEdge (P2 a)), EdgeRef, EdgeRef)
		-> ST s (Q.QEDS s (Edges (P2 a)), EdgeRef, EdgeRef)
addVoronoiNodes (q, e, r)
  = do q' <- Q.amap initAttr q           
       cs <- Q.connected q'
       foreach (map fst cs) (calculate q')
       updateOuterVertices q' e
       return (q', e, r)
  where
  initAttr e                  = e { attributes = Edges x y Infty Infty }
    where (DelEdge x y)       = attributes e 

  calculate :: (Ord a, Fractional a) => Q.QEDS s (Edges (P2 a)) -> Index -> ST s ()
  calculate q i
    = do let e = (i, Rot0, Normal)
  	 edge <- Q.getEdge q e
	 let (Edges o d r l) = attributes edge
	 ln <- lnext q e; lp <- lprev q e
	 rn <- rnext q e; rp <- rprev q e
	 l' <- testNode l o d ln lp
	 r' <- testNode r o d rp rn
	 Q.update q (i, edge { attributes = Edges o d r' l' })
    where
    testNode x@(Finite _) _ _  _ _ 
      = return x
    testNode Infty o d nxt prv
      = do d' <- dest q nxt; o' <- org q prv
  	   return (if d'==o' then node o d d' else Infty)
    node x y z                = maybe Infty Finite (intersect b1 b2)
      where b1                = bisector (Segment x y)
            b2                = bisector (Segment y z)

updateOuterVertices           :: Q.QEDS s (Edges (P2 a)) -> EdgeRef -> ST s ()
updateOuterVertices q e       = do hs <- ring q rprev e
				   if length hs <= 3 
				      then foreach_ hs makeInfty
				      else return ()
    where
    makeInfty x@(i, r, _)     = do a <- Q.getAttr q x
				   Q.updateAttr q x (update r a)
      where update Rot0 e     = e { left = Infty }
	    update Rot2 e     = e { right = Infty }
\end{code}

\begin{code}
voronoiEdges                  :: (Ord a, Floating a) => [Edges (P2 a)] -> [Line2 a]
voronoiEdges                  = catMaybes . map voronoiEdge

voronoiEdge                   :: (Fractional a, Ord a) => Edges (P2 a) -> Maybe (Line2 a)
voronoiEdge (Edges _ _ (Finite r) (Finite l))
		              = Just (Segment r l)
voronoiEdge (Edges _ _ Infty Infty)
                              = Nothing
voronoiEdge (Edges o d (Finite r) Infty)
  | isLeftTurn o d r	      = Just (Ray r (r + (r - c)))
  | otherwise		      = Just (Ray r c)
  where c		      = centerOfSegment (Segment o d)
voronoiEdge (Edges o d Infty (Finite l))
  | isLeftTurn o d l          = Just (Ray l c)
  | otherwise		      = Just (Ray l (l + (l - c)))
  where c		      = centerOfSegment (Segment o d)
\end{code}


data VoroEdge a  = VoroEdge { vleft, vright :: (Topped (P2 a)) }

voronoiF :: (Ord a, Fractional a) => [P2 a] -> S.QEDS (VoroEdge a)
voronoiF ps		      = array (bounds q) (map g (assocs q))
  where 
  (q, l, _)		      = delaunay ps

  g (i, Nothing)	      = (i, Nothing)
  g (i, Just (x@(Edge t (DelEdge o d))))
    | isConnected x           = (i, Just (Edge t (VoroEdge l r)))
    | otherwise		      = (i, Just (Edge t (VoroEdge Infty Infty)))
    where l		      = testNode o d (S.lnext q e) (S.lprev q e)
	  r		      = testNode o d (S.rprev q e) (S.rnext q e)
	  e		      = (i, Rot0, Normal)

  testNode o d nxt prv	      = if d'==o' then node o d d' else Infty
    where d'		      = D.target (S.getAttr q nxt)
          o'		      = D.source (S.getAttr q prv)
	  node x y z          = maybe Infty Finite (intersect b1 b2)
	    where b1          = bisector (Segment x y)
		  b2          = bisector (Segment y z)


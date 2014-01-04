%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Ein optimaler Divide \& Conquer Algorithmus (\texttt{Delaunay})}
\module{Delaunay}

aus \cite{guibas85}, siehe \cite{leach92:improving} und
\cite{shewchuk:triangle} für mögliche Verbesserungen.

\begin{code}
module Delaunay ( 
		  delaunay, delaunayST,
		  DelEdge (..), QEDS, org, dest, connect,
		  isLeftOf, isRightOf
                ) where

import QEDSbasics
import qualified QEDS as Q
import qualified QEDSstatic as S
import QEDS      ( lnext, rprev, onext, oprev, makeEdge, splice, deleteEdge )
import Divide    ( divideSorted )
import Point2    ( Point2 (..), P2, lexic2, orientation,
		   isRightTurn, isLeftTurn )
import qualified Point2 as Pt2 ( Orientation (..) )
import Point3    ( Point3 (..), volume6 )
import Control.Monad.ST ( ST )
import Prelude hiding (flip)
import Control.Monad.ST (runST)
import Control.Monad (liftM4)
import Circle
import Point (sqrDistance, xcoord, ycoord)
import Basics.Sorting   ( nubSort )
\end{code}

Anpassen der QEDS

\begin{code}
data DelEdge a		      = DelEdge { source, target :: a } deriving Show

type QEDS s a		      = Q.QEDS s (DelEdge a)

org, dest		      :: QEDS s a -> EdgeRef -> ST s a
org q x@(i, r, _)	      = do e <- Q.getAttr q x
				   return (if r==Rot0 then source e 
						      else target e)
dest q x@(i, r, _)	      = do e <- Q.getAttr q x
				   return (if r==Rot0 then target e
						      else source e)

connect			      :: QEDS s a -> EdgeRef -> EdgeRef -> ST s EdgeRef
connect q a b		      = do s <- dest q a; t <- org q b
				   Q.connect (\ x y -> DelEdge s t) q a b
\end{code}

Hilfsfunktionen

\begin{code}
isLeftOf, isRightOf	      :: (Ord a, Num a) 
			      => QEDS s (P2 a) -> P2 a -> EdgeRef -> ST s Bool
isLeftOf q x e 		      = do o<-org q e; d<-dest q e; return (isLeftTurn o d x)
isRightOf q x e               = do o<-org q e; d<-dest q e; return (isRightTurn o d x)

isValid			      :: (Ord a, Num a) 
                              => QEDS s (P2 a) -> EdgeRef -> EdgeRef -> ST s Bool
isValid q base e	      = do d<-dest q e; isRightOf q d base

inCircle		      :: (Ord a, Num a) => P2 a -> P2 a -> P2 a -> P2 a -> Bool
inCircle a b c d	      = volume6 (f a) (f b) (f c) (f d) > 0 
  where f (Point2 (x,y))      = Point3 (x, y, x^2 + y^2)

{- die naive Version
inCircle		      :: (Ord a, Fractional a) => P2 a -> P2 a -> P2 a -> P2 a -> Bool
inCircle a b c d	      = case circleFrom3Points a b c of
				  Nothing -> True
				  Just (Circle c r2) -> sqrDistance c d < r2
-}

instance (Ord a, Num a) => Ord (Point2 a) where
    compare		      = lexic2

inCircleM		      :: (Monad m, Num a, Ord a) => m (P2 a) -> m (P2 a) 
			      -> m (P2 a) -> m (P2 a) -> m Bool
inCircleM		      = liftM4 inCircle

ifM			      :: Monad m => m Bool -> m a -> m a -> m a
ifM p s t		      = do b<-p; if b then s else t
\end{code}

Der Algorithmus

\begin{code}
delaunay                      :: (Ord a, Num a) 
			      => [P2 a] -> (S.QEDS (DelEdge (P2 a)), EdgeRef, EdgeRef)
delaunay ps		      = runST (do (a,l,r) <- delaunayST ps
					  b <- Q.freeze a
					  return (b,l,r))

delaunayST		      :: (Ord a, Num a) => [P2 a] 
			      -> ST s (QEDS s (P2 a), EdgeRef, EdgeRef)
delaunayST ps		      = do a <- Q.empty
				   (l, r) <- delone a (nubSort ps)
				   return (a, l, r)

delone			      :: (Ord a, Num a) => QEDS s (P2 a) -> [P2 a] 
			      -> ST s (EdgeRef, EdgeRef)
delone q [s1,s2]	      = do a <- makeEdge q (DelEdge {source=s1, target=s2})
				   return (a, sym a)

delone q [s1,s2,s3]           = do a <- makeEdge q (DelEdge {source=s1, target=s2})
				   b <- makeEdge q (DelEdge {source=s2, target=s3})
				   splice q (sym a) b
				   case orientation s1 s2 s3 of 
				     Pt2.CounterClockwise -> do _ <- connect q b a
							        return (a, sym b) 
				     Pt2.Collinear  	  -> do return (a, sym b)
				     Pt2.Clockwise	  -> do c <- connect q b a
							        return (sym c, c) 

delone q ss@(_:_:_:_:_)	      = do (ldo, ldi) <- delone q ls
				   (rdi, rdo) <- delone q (es++hs)
				   (ldi, rdi) <- lowerBridge q ldi rdi
				   b <- connect q (sym rdi) ldi
  				   merge q b
			           ldo <- wrtBase ldi ldo (sym b)
				   rdo <- wrtBase rdi rdo b
				   return (ldo, rdo)
  where (m, (ls, es, hs))     = divideSorted ss
	wrtBase a b c	      = do x <- org q a; y <- org q b
				   return (if x == y then c else b)

lowerBridge		      :: (Ord a, Num a) => QEDS s (P2 a) -> EdgeRef 
			      -> EdgeRef -> ST s (EdgeRef, EdgeRef)
lowerBridge q el er	      = do l <- org q el; m <- dest q el
				   r <- org q er; n <- dest q er
				   check l m r n
  where check l m r n
	  | isRightTurn l r m = do el' <- lnext q el; lowerBridge q el' er
	  | isRightTurn l r n = do er' <- rprev q er; lowerBridge q el er'
	  | otherwise	      = do return (el,er)

merge                         :: (Ord a, Num a) => QEDS s (P2 a) -> EdgeRef -> ST s ()
merge q base		      = do l <- deleteEdges q base onext (sym base)
				   r <- deleteEdges q base oprev base
				   p1 <- isValid q base l
				   p2 <- isValid q base r
				   p3 <- inCircleM (dest q l) (org q l) (org q r) (dest q r)
				   check p1 p2 p3 l r
  where 
  check vl vr ic l r
    | not vl && not vr        = return ()
    | not vl || (vr && ic)    = do x <- connect q r (sym base)
				   merge q x
    | otherwise		      = do x <- connect q (sym base) (sym l)
				   merge q x

deleteEdges                   :: (Num a, Ord a) => QEDS s (P2 a) -> EdgeRef 
			      -> (QEDS s (P2 a) -> EdgeRef -> ST s EdgeRef) 
			      -> EdgeRef -> ST s EdgeRef
deleteEdges q base next b     = do e <- next q b
				   ifM (isValid q base e) (try e) (return e)
 where try e	              = do t <- next q e
				   ifM (inCircleM (dest q base) (org q base) 
				    	          (dest q e) (dest q t))
				       (do deleteEdge q e; try t)
				       (return e)
\end{code}





%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
\subsection{Die ``Quad-edge Datenstruktur'' (\texttt{QEDS})}
\module{QEDS}

\begin{code}
module QEDS (module QEDS, module QEDSbasics, module Basics.DynamicArray) where

import Prelude hiding (flip, lookup)
import QEDSbasics
import Control.Monad.ST ( ST )
import Basics.Utilities (fst3, foreach)
import Basics.STUtils (mkEmpty, contains, include, accumulate)
import Basics.DynamicArray 
import qualified Basics.DynamicArray as DA
\end{code}

\begin{code}
type QEDS s a                 = DA.Array s (Edge a)

getEdge			      :: QEDS s a -> EdgeRef -> ST s (Edge a)
getEdge q (i, _, _)	      = do DA.getThe q i

getAttr			      :: QEDS s a -> EdgeRef -> ST s a
getAttr q e                   = do x <- getEdge q e; return (attributes x)

updateEdge		      :: QEDS s a -> EdgeRef -> (Edge a) -> ST s ()
updateEdge q (i, _, _) e      = do DA.update q (i, e)

updateAttr		      :: QEDS s a -> EdgeRef -> a -> ST s ()
updateAttr q (i, _, _) a      = do e <- DA.getThe q i
				   DA.update q (i, e { attributes = a })

onext, oprev, lnext, lprev, 
  rnext, rprev, dnext, dprev  :: QEDS s a -> EdgeRef -> ST s EdgeRef
onext q (i, r, f)	      
  = do e <- DA.getThe q i
       let t = edgeTable e
       return ( if f==Normal then lookupET r t
			     else flip (rot (lookupET (incrDir r) t)) )

comp                :: (EdgeRef -> a) -> (b -> EdgeRef) -> QEDS s d -> b -> ST s a
comp g f qeds x     = do e <- onext qeds (f x); return (g e)
oprev               = rot `comp` rot
lnext               = rot `comp` rotInv
lprev               = sym `comp` id
rnext               = rotInv `comp` rot
rprev               = id `comp` sym
dnext               = sym `comp` sym
dprev               = rotInv `comp` rotInv 

makeEdge		      :: QEDS s a -> a -> ST s EdgeRef
makeEdge q a                  = do i <- DA.nextIndex q
				   DA.insert q (Edge { edgeTable = emptyET i, attributes = a })
				   return (i, Rot0, Normal)

splice			      :: QEDS s a -> EdgeRef -> EdgeRef -> ST s ()
splice q a b
  | (isPrimal a && isPrimal b) || (isDual a && isDual b) 
  = do oa <- onext q a
       if b == flip oa
	  then return ()
          else do -- !!! parallele Zuweisung !!!
		  ob <- onext q b
		  let x = rot oa; y = rot ob
		  ox <- onext q x; oy <- onext q y
		  update a y ob; update b x oa
		  update x b oy; update y a ox
  | otherwise = error "QEDS.splice: one primal and one dual edge"
  where
  update (i, r, Normal) _ n    
    = do e <- DA.getThe q i
         DA.update q (i, updEdge e r n)
  update (i, r, Flipped) z _   
    = do e <- DA.getThe q i
	 DA.update q (i, updEdge e (incrDir r) (flip z))
  updEdge e r v
    = e { edgeTable = updateET (edgeTable e) r v }

connect			      :: (a -> a -> a) -> QEDS s a -> EdgeRef -> EdgeRef -> ST s EdgeRef
connect f q a b        	      = do x <- getAttr q a; y <- getAttr q b
				   l <- lnext q a; e <- makeEdge q (f x y)
				   splice q e l; splice q (sym e) b
				   return e

deleteEdge		      :: QEDS s a -> EdgeRef -> ST s ()
deleteEdge q e		      = do x <- oprev q e; splice q e x
				   x <- oprev q s; splice q s x
  where s		      = sym e
\end{code}

\begin{code}
ring :: QEDS s a -> (QEDS s a -> EdgeRef -> ST s EdgeRef) -> EdgeRef -> ST s [EdgeRef]
ring q f e@(i, _, _) 
  = next e
  where
  next e = do e'@(j, _, _) <- f q e
              if j/=i then do xs <- next e'; return (e:xs)
		      else return [e]

components :: QEDS s a -> ST s [[Index]]
components q 
  = do as <- DA.assocs q
       let bs = map (\ (i, _) -> (i, Rot0, Normal)) as
       a <- mkEmpty (fst (head as), fst (last as))
       rs <- accumulate (map (\ i -> visit a [i]) bs)
       return (filter (/=[]) rs)
  where
  visit a [] = return []
  visit a (e@(i,_,_):es)
    = do n <- contains a i
         if n then visit a es
	       else do include a i 
		       o <- onext q e; d <- dnext q e
		       is <- visit a (o : d : es)
		       return (i:is)

connected, unconnected        :: QEDS s a -> ST s [(Index, Edge a)]
connected q  		      = do as <- DA.assocs q; return (filter (isConnected . snd) as)
unconnected q  		      = do as <- DA.assocs q; return (filter (isUnconnected . snd) as)

someConnectedEdge             :: QEDS s a -> ST s (Index, Edge a)
someConnectedEdge q           = do cs <- connected q; return (head cs)
\end{code}

\begin{code}
nodes			      :: Direction -> QEDS s a -> ST s [EdgeRef]
nodes r q	              = do s <- DA.size q
				   vs <- mkEmpty (1,s)
				   qs <- connected q
				   let (i,_) = head qs
				   let e = (i, r, Normal)
				   tmp <- ring q onext e
				   let rs = map sym tmp
				   xs <- visit vs rs
				   return (e:xs)
  where 
  visit vs []		      = return []
  visit vs (e@(i,_,_):es)     = do tmp <- ring q onext e
				   let rs = map sym tmp
				   let is = map fst3 rs
				   xs <- foreach is (contains vs)
				   include vs i
				   if or xs
				      then visit vs es
				      else do xs <- visit vs (tail rs ++ es)
					      return (e:xs)
\end{code}


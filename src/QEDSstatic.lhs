%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
\subsubsection{Die statische Version (\texttt{QEDSstatic})}
\module{QEDSstatic}

\begin{code}
module QEDSstatic (module QEDSstatic, module QEDSbasics) where

import Prelude hiding (flip, lookup)
import QEDSbasics
import qualified Data.Array as A
import qualified Basics.Pretty as Pr
import Control.Monad.ST ( ST )
import Control.Monad.ST (runST)
import Basics.Utilities (fst3, foreach)
import Basics.STUtils (mkEmpty, contains, include, accumulate)
\end{code}

\begin{code}
type QEDS a                   = A.Array Index (Maybe (Edge a))

size			      :: QEDS a -> Index
size 			      = snd . A.bounds

lookup                        :: QEDS a -> Index -> Maybe (Edge a)
lookup                        = (A.!)

getThe                        :: QEDS a -> Index -> Edge a
getThe q i                    = case lookup q i of
                                  Nothing -> error "QEDSstatic.getThe"
                                  Just x -> x

getEdge			      :: QEDS a -> EdgeRef -> Edge a
getEdge q (i, _, _)	      = getThe q i

getAttr			      :: QEDS a -> EdgeRef -> a
getAttr q r                   = attributes (getEdge q r)

assocs                        :: QEDS a -> [(Index, Edge a)]
assocs q		      = [ (a,b) | x@(a, Just b) <- A.assocs q ]

elems                         :: QEDS a -> [Edge a]
elems                         = map snd . assocs

indices                       :: QEDS a -> [Index]
indices                       = map fst . assocs
\end{code}

\begin{code}
{-
org, dest                     :: QEDS (DelEdge a) -> EdgeRef -> a
org q x@(i, r, _)             = f r (getAttr q x)
  where f Rot0                = source
        f Rot2                = target
dest q x@(i, r, _)            = f r (getAttr q x)
  where f Rot2                = source
        f Rot0                = target

orgRot, destRot               :: QEDS a-> EdgeRef -> f
orgRot q (i, r, f)            = f r (getThe q i)
  where f Rot1                = rotOrg 
        f Rot3                = rotDest
destRot q (i, r, f)           = f r (getThe q i)
  where f Rot3                = rotOrg 
        f Rot1                = rotDest
-}

onext, oprev, lnext, lprev, 
  rnext, rprev, dnext, dprev  :: QEDS a -> EdgeRef -> EdgeRef

onext q (i, r, f)             = case f of
				  Normal  -> lookupET r t
				  Flipped -> flip (rot (lookupET (incrDir r) t))
  where t		      = edgeTable (getThe q i)
       
comp g f q x                  = g (onext q (f x))
oprev                         = rot `comp` rot
lnext                         = rot `comp` rotInv
lprev                         = sym `comp` id
rnext                         = rotInv `comp` rot
rprev                         = id `comp` sym
dnext                         = sym `comp` sym
dprev                         = rotInv `comp` rotInv 
\end{code}

\begin{code}
ring :: QEDS a -> (QEDS a -> EdgeRef -> EdgeRef) -> EdgeRef -> [EdgeRef]
ring q f e@(i,_,_) 
  = e : takeWhile (\ (j,_,_) -> j/=i) (s e)
  where s e = let e' = f q e in e' : s e'

components :: QEDS a -> [[Index]]
components q 
  = filter (/=[]) ( runST (do a <- mkEmpty (fst (head as), fst (last as))
			      accumulate (map (\ i -> visit a [i]) bs) ))
  where
  as = assocs q
  bs = map (\ (i, _) -> (i, Rot0, Normal)) as

  visit a [] = return []
  visit a (e@(i,_,_):es)
    = do n <- contains a i
         if n then visit a es
	       else do include a i
		       is <- visit a (onext q e : dnext q e : es)
		       return (i:is)

connected, unconnected        :: QEDS a -> [(Index, Edge a)]
connected   		      = filter (isConnected . snd) . assocs
unconnected   		      = filter (isUnconnected . snd) . assocs

someConnectedEdge             :: QEDS a -> (Index, Edge a)
someConnectedEdge             = head . connected
\end{code}

Ermittle die Liste der Knoten einer QEDS |q| mit einem Tiefendurchlauf. Zurückgegeben wird eine
Liste von Kantenreferenzen |e|, so daß |v == org q e| ist.

\begin{code}
nodes			      :: Direction -> QEDS a -> [EdgeRef]
nodes r q	              = runST
	 		      ( do let s = size q
				   vs <- mkEmpty (1,s)
				   let (i,_) = head (connected q)
				   let e = (i, r, Normal)
				   let rs = map sym (ring q onext e)
				   xs <- visit vs rs
				   return (e:xs) )
  where 
  visit vs []		      = return []
  visit vs (e@(i,_,_):es)     = do xs <- foreach is (contains vs)
				   include vs i
				   if or xs
				      then visit vs es
				      else do xs <- visit vs (tail rs ++ es)
					      return (e:xs)
    where rs		      = map sym (ring q onext e)
	  is		      = map fst3 rs
\end{code}


\subsubsection*{Pretty-Printing}

\begin{code}
{-
pprint :: (Edge a -> Pr.Doc) -> QEDS a -> String
generic_ppr ppr q = Pr.render (Pr.vcat (map print (assocs q)))
  where print (i,e) = Pr.text (show i ++ " ") Pr.<+> ppr e

pprintP :: Show a => QEDS a -> String
pprP = generic_ppr pprEdge_primal

pprD :: Show f => QEDS a -> String
pprD = generic_ppr pprEdge_dual

pprB :: (Show p, Show f) => QEDS p f d -> String
pprB = generic_ppr (\ e -> pprEdge_primal e Pr.<+> pprEdge_dual e)
-}
\end{code}




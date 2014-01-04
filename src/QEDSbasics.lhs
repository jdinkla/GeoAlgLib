%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
\subsubsection{Definitionen für die QEDS (\texttt{QEDSbasics})}
\module{QEDSbasics}

\begin{code}
module QEDSbasics (module QEDSbasics) where

import Prelude hiding (flip)
import Data.List (nub) 
import Basics.Topped (Topped (..))
import Basics.Utilities (fst4, snd4, thd4, frt4, fst3, isSingleton)
import qualified Basics.Pretty as Pr

type Index		      = Int

data Edge a                   = Edge { edgeTable :: EdgeTable, 
                                       attributes :: a }
                                deriving (Eq, Show)

type EdgeRef                  = (Index, Direction, Orientation)
type EdgeTable		      = (EdgeRef, EdgeRef, EdgeRef, EdgeRef)

data Direction                = Rot0 | Rot1 | Rot2 | Rot3 
                                deriving (Eq, Ord, Show)

data Orientation              = Normal | Flipped
				deriving (Eq, Ord, Show)

incrDir, decrDir              :: Direction -> Direction
incrDir x		      = case x of { Rot0 -> Rot1; Rot1 -> Rot2; 
					    Rot2 -> Rot3; Rot3 -> Rot0 }
decrDir x		      = case x of { Rot0 -> Rot3; Rot1 -> Rot0; 
					    Rot2 -> Rot1; Rot3 -> Rot2 }

edgesET			      :: EdgeTable -> [EdgeRef]
edgesET (a,b,c,d)	      = [a,b,c,d]

emptyET                       :: Index -> EdgeTable
emptyET i		      = (mk Rot0, mk Rot3, mk Rot2, mk Rot1)
  where mk r		      = (i, r, Normal)

lookupET		      :: Direction -> EdgeTable -> EdgeRef
lookupET Rot0 (x,_,_,_)       = x
lookupET Rot1 (_,x,_,_)       = x
lookupET Rot2 (_,_,x,_)       = x
lookupET Rot3 (_,_,_,x)       = x

updateET		      :: EdgeTable -> Direction -> EdgeRef -> EdgeTable
updateET (_,b,c,d) Rot0	v     = (v,b,c,d)
updateET (a,_,c,d) Rot1 v     = (a,v,c,d)
updateET (a,b,_,d) Rot2 v     = (a,b,v,d)
updateET (a,b,c,_) Rot3 v     = (a,b,c,v)

isPrimal, isDual              :: EdgeRef -> Bool
isPrimal (_, r, _)            = r == Rot0 || r == Rot2
isDual (_, r, _)              = r == Rot1 || r == Rot3

isFlipped                     :: EdgeRef -> Bool
isFlipped (_, _, f)           = f == Flipped

rot, sym, rotInv, flip        :: EdgeRef -> EdgeRef
rot (e, r, Normal)	      = (e, incrDir r, Normal)
rot (e, r, Flipped)	      = (e, decrDir r, Flipped)

rotInv (i, r, Normal)	      = (i, decrDir r, Normal)
rotInv (i, r, Flipped)        = (i, incrDir r, Flipped)

sym			      = rot . rot

flip (e, r, Normal)	      = (e, r, Flipped)
flip (e, r, Flipped)	      = (e, r, Normal)

isConnected, isUnconnected    :: Edge a -> Bool
isConnected		      = not . isUnconnected
isUnconnected       	      = isSingleton . nub . map fst3 . edgesET . edgeTable
\end{code}


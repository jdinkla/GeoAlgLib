%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Kirkpatrick und Seidels Algorithmus (|KirkpatrickSeidelHull|)}
\module{KirkpatrickSeidelHull}

siehe \cite{kirkpatrick86}

\begin{code}
module KirkpatrickSeidelHull ( 
         ksHull, ksHullX
       )
where

import Point2    ( Point2, P2, xcoord, ycoord, lessX, lessY, greaterY, 
		   lowestLeftest, lowestRightest, highestLeftest, highestRightest,
		   isRightTurn, isLeftTurn, leftest, rightest, clockwise3 )
import Point     ( inIntervalX )
import Line      ( Line (Segment), slope, Slope (..) )
import Polygon   ( Polygon (PolygonCW), Polygon2 )
import Basics.Sorting   ( partition3 )
import Basics.OrderStat ( median )
import Basics.Utilities ( minimaWith, maximaWith )
import Basics.ListCat

--infixl 2 ~~

ksHull                        :: (Ord a, Fractional a) => [P2 a] -> Polygon2 a
ksHull                        = ksHullX median median

ksHullX                       :: (Ord a, Fractional a) => ([a] -> a) -> ([a] -> a) 
			      -> [P2 a] -> Polygon2 a
ksHullX med1 med2 ps@(_:_:_:_)= PolygonCW (upper ++ reverse (remove lower))
  where ll                    = lowestLeftest ps
        lr                    = lowestRightest ps
        ul                    = highestLeftest ps
        ur                    = highestRightest ps
        lower | ll == lr      = [ll]
	      | otherwise     = list (connect Lower med1 med2 ll lr (ll:lr:inBetween))
        upper | ul == ur      = [ul]
	      | otherwise     = list (connect Upper med1 med2 ul ur (ul:ur:inBetween))
        inBetween             = filter (`inIntervalX` (ll,lr)) ps

        remove                = if ll == ul then tail else id
			      . if lr == ur then init else id

ksHullX _ _ ps                = PolygonCW (clockwise3 ps)

data Mode                     = Upper | Lower deriving Show

connect                       :: (Ord a, Fractional a) => Mode -> ([a] -> a) 
			      -> ([a] -> a) -> P2 a -> P2 a  -> [P2 a] -> [P2 a] -> [P2 a]
connect m med1 med2 l r ps
  | l == r                    = unit l
  | otherwise                 = connect m med1 med2 l i ls
                                `cat` connect m med1 med2 j r rs
  where a                     = med1 (map xcoord ps) 
        (i,j)                 = bridge m med2 ps a
        ls		      = l:i:filter (\ x -> x `lessX` i && turn m l i x) ps
        rs                    = r:j:filter (\ x -> j `lessX` x && turn m j r x) ps
        turn Upper	      = isLeftTurn
	turn Lower	      = isRightTurn

type Pair a                   = (P2 a, P2 a)

bridge                        :: (Ord a, Fractional a) => Mode -> ([a] -> a) 
			      -> [P2 a] -> a -> (Pair a)
bridge m med [i,j] a          = orderedPair i j
bridge m med ps a
  | null pairs                = bridge m med (cand1 ++ cand2) a
  | xcoord l <= a 
    && xcoord r > a	      = (l, r)
  | otherwise                 = bridge m med (cand1 ++ cand2 ++ candrest) a
  where (tmp, cand1)          = intoPairs ps []
        (pairs, cand2)        = slopes m tmp
        k                     = med (map snd pairs)
        extremeWrtY           = extreme m k ps
        l                     = leftest extremeWrtY
        r                     = rightest extremeWrtY
        candrest              = remaining l r a parts
	  where parts         = (map fst as, map fst bs, map fst cs)
	        (as, bs, cs)  = partitionWrtSlope m k pairs

intoPairs                     :: (Ord a, Num a) => [P2 a] -> [Pair a] 
			      -> ([Pair a], [P2 a])
intoPairs [] acc              = (acc, [])
intoPairs [p] acc             = (acc, [p])
intoPairs (p:q:ps) acc        = intoPairs ps (orderedPair p q : acc)

orderedPair                   :: (Ord a, Num a) => P2 a -> P2 a -> (P2 a, P2 a)
orderedPair i j               = if xcoord i <= xcoord j then (i,j) else (j,i)

slopes                        :: (Ord a, Fractional a) => Mode -> [Pair a] 
			      -> ([(Pair a,a)],[P2 a])
slopes m                      = foldl filter ([],[]) 
  where filter (ns,cs) (p,q)  = case slope (Segment p q) of 
                                  Vertical -> (ns, select m : cs)
                                  Slope sl -> (((p,q), sl):ns, cs)
          where select Upper  = if p `greaterY` q then p else q
	        select Lower  = if p `lessY` q then p else q

partitionWrtSlope             :: (Ord a, Fractional a) => Mode -> a -> [(b,a)] 
			      -> ([(b,a)],[(b,a)],[(b,a)])
--partitionWrtSlope Upper k ps  = partition3 (\ x -> (snd x) ~~ k) ps
partitionWrtSlope Upper k ps  = partition3 (\ x -> compare (snd x) k) ps
--partitionWrtSlope Lower k ps  = partition3 (\ x -> k ~~ (snd x)) ps
partitionWrtSlope Lower k ps  = partition3 (\ x -> compare k (snd x)) ps

--(~~)		  	      :: (Ord a, Fractional a) => a -> a -> Ordering
--x ~~ y			      = compareEps 1.0e-5 x y

extreme                       :: (Ord a, Fractional a) => Mode -> a -> [P2 a] -> [P2 a]
extreme m k ps		      = case m of
--				  Upper -> maximaWithBy (~~) key ps
				  Upper -> maximaWith key ps
--				  Lower -> minimaWithBy (~~) key ps
				  Lower -> minimaWith key ps
  where key x		      = ycoord x - k * xcoord x

remaining                     :: (Ord a, Num a) => P2 a -> P2 a -> a 
			      -> ([Pair a], [Pair a], [Pair a]) -> [P2 a]
remaining l r a (ls, es, hs)
  | xcoord r <= a             = flatten ls ++ map snd (es++hs)
  | xcoord l > a              = map fst (ls++es) ++ flatten hs 
  | otherwise                 = []
  where flatten               = foldr (\ (x,y) xs -> x:y:xs) [] 
\end{code}



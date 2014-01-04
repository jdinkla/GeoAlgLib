%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
\subsection{Der Delaunay-DAG (|DelaunayDAG|)}
\module{DelaunayDAG}

\begin{code}
module DelaunayDAG (
         DDAG, StaticDDAG, Simplex (..), Node (..), 
	 initDDAG, insertSite, delaunay,
	 isInConflict, isHalfplane, vertices, triangles
       ) where

import Point2    ( Point2, P2, isLeftTurn, isRightTurn, leftOf, sqrDistance )
import Circle    ( circleFrom3Points )
import Data.Array     ( Array, (!) )
import Control.Monad     ( unless )
import Data.Maybe     ( catMaybes )
import qualified Circle as C
import qualified Triangle as Tri
import Prelude   hiding ( lookup )
import Control.Monad.ST ( ST )
import Control.Monad.ST (runST)
import Basics.Utilities ( foreach, foreach_ )
import Basics.STUtils   ( Set, mkEmpty, contains, include )
import Basics.Sorting   ( isortBy )
import Basics.DynamicArray (getThe, empty, update, insert, size, freeze)
import qualified Basics.DynamicArray as DA

--import qualified Trace
--msg str = strict Trace.trace (str++"\n") return ()
msg str = return ()

--
--
--
data Simplex a
  = Halfplane { p1, p2 :: Point2 a }
  | Triangle  { center :: Point2 a, sqrRadius :: a, p1, p2, p3 :: Point2 a }

vertices		      :: Simplex a -> [Point2 a]
vertices (Halfplane p q)      = [p, q]
vertices (Triangle _ _ p q r) = [p, q, r]

data Node a 
  = Node { simplex :: Simplex a, dead :: Bool, neighbours :: Neighbours,
           sons, stepsons :: [Index] }

type Neighbours		      = (Index, Index, Index)
type Index		      = Int
type DDAG s a		      = DA.Array s (Node a)
type StaticDDAG a	      = Array Index (Maybe (Node a))

neighboursList		      :: Node a -> [Index]
neighboursList e	      = let (n1,n2,n3) = neighbours e in [n1,n2,n3]
            
getWith		              :: DDAG s a -> (Node a -> b) -> Index -> ST s b
getWith dag f i		      = do e_i <- getThe dag i; return (f e_i)

updateWith		      :: DDAG s a -> (Node a -> Node a) -> Index -> ST s ()
updateWith dag f i            = do e_i <- getThe dag i; update dag (i, (f e_i))

--
--
--
initDDAG		      :: (Ord a, Fractional a) => [P2 a] -> ST s (DDAG s a)
initDDAG ps@[_, _, _]         = do dag <- empty
				   foreach [n1, n2, n3, n4] (insert dag) 
				   return dag
  where ss@[p, q, r]	      = ccw ps
	n1		      = node {simplex=triangle ss, neighbours=(2,3,4)}
	n2		      = node {simplex=Halfplane p q, neighbours=(4,3,1)}
	n3		      = node {simplex=Halfplane q r, neighbours=(2,4,1)}
	n4		      = node {simplex=Halfplane r p, neighbours=(3,2,1)}

node			      :: Node a 
node			      = Node { dead = False, neighbours = (0,0,0), 
				       sons = [], stepsons = [] }

ccw			      :: (Ord a, Num a) => [P2 a] -> [P2 a]
ccw [p,q,r]
  | isLeftTurn p q r	      = [p,q,r] 
  | otherwise		      = [p,r,q]

triangle		      :: (Ord a, Fractional a) => [P2 a] -> Simplex a
triangle [p,q,r]              = case (circleFrom3Points p q r) of
				  Nothing -> error "collinear points"
				  Just c  -> Triangle (C.center c) (C.radius c) p q r

isInConflict		      :: (Ord a, Num a) => Point2 a -> Simplex a -> Bool
isInConflict p (Halfplane s t) = isRightTurn s t p
isInConflict p (Triangle c r2 _ _ _) = sqrDistance p c < r2

--
-- INSERTSITE
--

insertSite :: (Ord a,Fractional a, Show a) => DDAG s a -> P2 a -> ST s ()
insertSite dag p 
  = do msg ("%insertSite " ++ show p)
       ks <- locate dag p
       foreach ks (updateWith dag (\ n -> n { dead = True }))
       foreach_ ks (create dag p)
       foreach_ ks (updateNeighbours dag)

--
-- LOCATE
--
-- locate ermittelt die Konfliktdreiecke bzgl. p und gibt diese zurück
locate :: (Num a, Ord a) => DDAG s a -> P2 a -> ST s [Index]
locate dag p 
  = do s <- size dag
       ss <- mkEmpty (1,s)
       find ss [1,2,3,4]
  where
  find ss [] = return []
  find ss (i:is)
    = do e_i <- getThe dag i
         visited <- contains ss i
         if not visited && p `isInConflict` (simplex e_i)
            then do include ss i
                    ks <- find ss (stepsons e_i ++ sons e_i ++ is)
                    return (if not (dead e_i) then (i:ks) else ks)
            else find ss is

--
-- CREATE
--
create :: (Ord a, Fractional a) => DDAG s a -> P2 a -> Index -> ST s ()
create dag p v
  = do e_v <- getThe dag v
       foreach_ (neighboursList e_v) (createAndConnect (simplex e_v))
  where
  createAndConnect n s 
    = do e_s <- getThe dag s
	 unless (dead e_s) (
	   do i <- createSimplex dag p n (simplex e_s)
              connect dag s v i
           )

createSimplex		      :: (Ord a, Fractional a) => DDAG s a -> P2 a
			      -> Simplex a -> Simplex a -> ST s Index
createSimplex dag p v s	      = insert dag (node { simplex = new ps })
  where ps		      = case commonFacet v s of
				  [x,y] -> [p,x,y]
				  -- zwei Halbebenen
                                  [z]   -> if isLeftTurn z p (otherPoint v z)
					   then [z,p] else [p,z]
        new [p,q]	      = Halfplane p q
	new ps@[_,_,_]	      = triangle (ccw ps)
        otherPoint (Halfplane p q) x = if x==p then q else p

isHalfplane                   :: Simplex a -> Bool
isHalfplane (Halfplane _ _)   = True
isHalfplane _		      = False

-- garantiert, daß die Punkte in der Ausgabe in v direkt aufeinanderfolgen
commonFacet                   :: (Num a, Eq a) => Simplex a -> Simplex a -> [P2 a]
commonFacet v s		      
  | isHalfplane v	      = reverse (g ms)
  | otherwise		      = g ms
  where ps		      = vertices s
	f x		      = if x `elem` ps then Just x else Nothing
	ms		      = map f (vertices v) 
	g [Just x, Nothing, Just y] = [y,x]
	g xs		      = catMaybes xs

connect :: DDAG s a -> Index -> Index -> Index -> ST s ()
connect dag s v i 
  = do updateWith dag (\ n -> n { sons = i : sons n }) v
       updateWith dag (\ n -> n { stepsons =  i : stepsons n,
                                  neighbours = new (neighbours n) }) s
  where new (n1, n2, n3)
          | n1 == v	      = ( i, n2, n3)
	  | n2 == v	      = (n1,  i, n3)
	  | n3 == v	      = (n1, n2,  i)

--
-- UPDATENEIGHBOURS
--
updateNeighbours :: (Num a, Eq a) => DDAG s a -> Index -> ST s ()
updateNeighbours dag v
  = do e_v <- getThe dag v
       foreach_ (neighboursList e_v) (check e_v)
  where
  check e_v s 
    = do e_s <- getThe dag s
	 unless (dead e_s) (
	   do l  <- follow dag nextCCW s v
	      r  <- follow dag nextCW s v
	      let xs@(x:_) = commonFacet (simplex e_v) (simplex e_s)
	      c <- commonNeighbour dag e_s xs
	      e_l <- getThe dag l
	      e_r <- getThe dag r
  	      let (n1, n2) = next xs e_l e_r c
	      updateWith dag (\ n -> n { neighbours = mkNbs x s n1 n2 n }) c
	    )

follow :: DDAG s a -> (Neighbours -> Index -> Index) -> Index -> Index -> ST s Index
follow dag next j i 
    = do e_i <- getThe dag i
         let n = next (neighbours e_i) j
	 e_n <- getThe dag n
	 if (dead e_n) then follow dag next i n
		       else return n

nextCCW, nextCW               :: Neighbours -> Index -> Index
nextCCW (n1,n2,n3) s 
  | n1 == s	              = n3
  | n2 == s		      = n1
  | n3 == s		      = n2

nextCW (n1,n2,n3) s
  | n1 == s	              = n2
  | n2 == s		      = n3
  | n3 == s		      = n1

-- ermittelt einen Nachbar von |e_i| der die Ecken in |ps| enthält.
commonNeighbour :: (Num a, Eq a) => DDAG s a -> Node a -> [P2 a] -> ST s Index
commonNeighbour dag e_i ps
  = do xs <- foreach ns (\ i -> do e_i <- getThe dag i
				   return (vertices (simplex e_i)))
       return (fst (head (filter allElem (zip ns xs))))
  where ns	              = neighboursList e_i
	allElem (_, xs)	      = all (`elem` xs) ps

next :: (Num a, Eq a) => [P2 a] -> Node a -> Node a -> Index -> (Index, Index)
next xs e_l e_r c	      = nxt xs
  where nxt [x,y]	      = (left x, right y)
	nxt [x]		      = ( if l==c then edgeX s_l n_l x else l, 
			          if r==c then edgeX s_r n_r x else r )
          where (l,r)	      = (left x, right x)
	s_l		      = simplex e_l
	s_r		      = simplex e_r
	n_l		      = neighbours e_l
	n_r		      = neighbours e_r
	left x		      = edgeL s_l n_l x
	right x		      = edgeR s_r n_r x

edgeL,edgeR, edgeX            :: (Num a, Eq a) => Simplex a -> Neighbours -> P2 a -> Index
edgeL (Halfplane s t) (n1,n2,n3) p	-- die danach
  | p == s                    = n1
  | p == t		      = n3
  | otherwise		      = n2
edgeL (Triangle _ _ s t u) (n1,n2,n3) p
  | p == s                    = n1
  | p == t		      = n2
  | p == u		      = n3

edgeR (Halfplane s t) (n1,n2,n3) p	-- die davor
  | p == s                    = n3
  | p == t		      = n2
  | otherwise		      = n1
edgeR (Triangle _ _ s t u) (n1,n2,n3) p
  | p == s                    = n3
  | p == t		      = n1
  | p == u		      = n2

edgeX (Halfplane s t) (n1,n2,n3) p	-- die andere
  | p == s                    = n2
  | p == t		      = n1
  | otherwise		      = n3

mkNbs                         :: (Num a, Eq a) => P2 a -> Index -> Index -> Index -> Node a -> Neighbours
mkNbs p s n1 n2 x	      = chk (vertices (simplex x))
  where chk (p1:p2:_)
	  | p1 == p	      = (s, n2, n1)
 	  | p2 == p	      = (n1, s, n2)
  	  | otherwise 	      = (n2, n1, s)

--
-- DELAUNAY
--
delaunay		      :: (Ord a, Fractional a, Show a) => [P2 a] -> StaticDDAG a
delaunay ps		      = runST (do dag <- initDDAG (take 3 ps)
					  foreach (drop 3 ps) (insertSite dag)
					  freeze dag)

triangles		      :: Num a => StaticDDAG a -> [Tri.Triangle Point2 a]
triangles d	              = find [1,2,3,4]
    where find []	      = []
	  find (i:is)
	    | dead e	      = find is'
	    | otherwise	      = case simplex e of
				  Triangle c s p q r -> Tri.Triangle (p,q,r) : find is'
				  _		       -> find is'
	    where (Just e)    = d!i
		  is'         = sons e ++ is
\end{code}

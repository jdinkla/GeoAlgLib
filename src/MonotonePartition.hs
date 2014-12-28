--------------------------------------------------------------------------------
-- Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
--------------------------------------------------------------------------------
--
-- see
--     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
--     University of Bonn, Germany, 1998.
--

--siehe \cite[K. 2.2]{orourke94:cg}, \cite[K. 3.2]{berg97:cg}

module MonotonePartition (
         monotonePartition, diagonals, Vertex (..), classify
       )
where

import Point2    ( Point2 (Point2), P2, xcoord,
           belowOrRight, aboveOrLeft, isLeftTurn, xcoord, ycoord )
import Line      ( Line (Segment), Line2, intersect, horizontal )
import Polygon   ( Polygon (..), Polygon2, vertices, angles, edges )
import Data.Array     ( listArray, (!), Array, bounds )
import Data.Maybe     ( fromJust, isJust )
import qualified Basics.FiniteMap as FM
import Basics.FiniteMap ( lookup, delete, insert, insertManyC, fromList, empty )
import qualified StatusStructureSpec as SS
import Prelude   hiding (lookup)
import Basics.ListCat   ( list, unit, cat )
import Basics.Sorting   ( sort, sortBy )
import Basics.Utilities ( relToFst, thd3 )

-- Aufteilung des Polygons anhand der Diagonalen in Teilpolygone in $O(n\log n)$.

monotonePartition :: (Ord a, Fractional a) => Polygon2 a -> [Polygon2 a]
monotonePartition poly = list (subpolys 0 n es ds [])
  where (arr, is) = diagonalRefs poly
        (_, n)    = bounds arr :: (Int, Int)
        es        = fromList (edges [0 .. n])
        ds        = insertManyC (++) empty $ map (\ (x,y) -> (x,[y]))
                                           $ sort (map orderedPair is)

        subpolys i j es ds ps
            | i == j = unit (PolygonCW (x:ps))
            | otherwise = case lookup i ds of
                               Nothing -> subpolys n j es ds (x:ps)
                               Just ts -> sub es ts
            where (Just n) = lookup i es
                  x        = arr ! i
                  ds'      = delete i ds

                  sub es [] = subpolys n j es ds' (x:ps)
                    where (Just n) = lookup i es

                  sub es (t:ts) = sub es_1 ts `cat` subpolys n t es_2 ds' [x]
                    where (Just n) = lookup i es
                          es_1     = insert i t es
                          es_2     = insert t i es

orderedPair              :: Ord a => (a,a) -> (a,a)
orderedPair (x,y)          = if x<=y then (x,y) else (y,x)

-- Die Typen von Knoten, nach db97, S. 52.

data Vertex a                 = Start (P2 a) | End (P2 a) | Split (P2 a)
                  | Merge (P2 a) | Regular (P2 a)
                deriving Show

point                  :: Vertex a -> Point2 a
point (Start x)              = x
point (End x)              = x
point (Split x)              = x
point (Merge x)              = x
point (Regular x)          = x

isMergeVertex                 :: Vertex a -> Bool
isMergeVertex (Merge _)       = True
isMergeVertex _               = False

classify              :: (Num a, Ord a) => (P2 a, P2 a, P2 a) -> Vertex a
classify (m,x,p)
  | m `belowOrRight` x
    && p `belowOrRight` x     = if isLeftTurn m x p then Start x else Split x
  | m `aboveOrLeft` x
    && p `aboveOrLeft` x      = if isLeftTurn m x p then End x else Merge x
  | otherwise                 = Regular x

type Ref = Int
type SS a = SS.SS Ref (Line2 a)
type HS = FM.FiniteMap Ref Ref
type ES a = [(Vertex a, Ref)]

diagonals :: (Ord a, Fractional a) => Polygon2 a -> [Line2 a]
diagonals poly = map toSegment rs
  where (arr, rs) = diagonalRefs poly
        toSegment (x, y) = Segment (arr!x) (arr!y)

diagonalRefs :: (Ord a, Fractional a) => Polygon2 a -> (Array Int (P2 a), [(Ref, Ref)])
diagonalRefs poly = (fmap point arr, sweep arr n ss hs es)
  where classified = fmap classify (angles (vertices poly))
        n          = length classified
        arr        = listArray (0,n-1) classified
        es         = sortBy (relToFst cmp) (zip classified [0..])
          where cmp x y = point x `aboveOrLeft` point y
        ss                    = SS.empty
        hs                    = FM.empty

--nach db97, S. 53f

sweep                  :: (Ord a, Fractional a) => Array Ref (Vertex a) -> Int
                  -> SS a -> HS -> ES a -> [(Ref, Ref)]
sweep arr n ss hs es          = thd3 (foldl step (ss, hs, []) es)
  where step s@(ss,hs,rs) (c_i, i)  = handle c_i
          where v_i = point c_i
                im  = pred i `mod` n
                ip  = succ i `mod` n

                handle (Start _)          = insert s
                handle (End _)            = leftUpdate s
                handle (Split _)          = insert (ss, FM.insert l i hs, (i, h_l):rs)
                  where (l, h_l)          = edgeToTheLeft ss
                handle (Merge _)          = rightUpdate (leftUpdate s)
                handle (Regular _)
                  | isOnLeftChain         = insert (leftUpdate s)
                  | otherwise          = rightUpdate s

                isOnLeftChain          = isJust (SS.lookup ss im)

                leftUpdate (ss, hs, rs)   = (SS.delete ss im, hs, checkMerge h_im rs)
                  where h_im          = fromJust (FM.lookup im hs)

                rightUpdate (ss, hs, rs)  = (ss, FM.insert l i hs, checkMerge h_l rs)
                  where (l, h_l)          = edgeToTheLeft ss

                checkMerge x xs          = if isMergeVertex (arr!x) then (i,x):xs else xs

                insert (ss, hs, rs)       = (ss', FM.insert i i hs, rs)
                  where ss'              = SS.insertBy cmp ss (i, Segment v_i v_ip)
                        v_ip              = point (arr ! ip)

                edgeToTheLeft ss          = (l, h_l)
                  where h_l                = fromJust (FM.lookup l hs)
                        l                 = SS.key (fromJust (SS.predBy cmp ss i))

                cmp _ (_, seg)          = compare (xcoord v_i) (xcoord inters)
                  where inters               = fromJust (intersect seg (horizontal (ycoord v_i)))

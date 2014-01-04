%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%

\begin{code}
module StatusStructureSpec (
         SS, empty, isEmpty, fromList, toList, key, value,
         lookup, insert, insertBy, insertMany, delete, deleteBy, deleteMany,
         pred, predBy, succ, succBy, swap
       ) 
where

import Basics.Utilities (relToFst, OrderRel, Rel, lessRel, equalRel, greaterRel)
import Basics.Sorting (sortBy)
--import Prelude hiding (lookup, head, tail, print, pred, succ)
import Prelude hiding (lookup, pred, succ)
import qualified List (insertBy, deleteBy)

newtype Ord a => SS a b       = SS [(a,b)] deriving Show

key			      :: (a,b) -> a
key                           = fst

value			      :: (a,b) -> b
value                         = snd

compareKeys                   :: Ord a => OrderRel (a,b)
compareKeys x y               = compare (key x) (key y)

empty                         :: Ord a => SS a b
empty                         = SS []

isEmpty                       :: Ord a => SS a b -> Bool
isEmpty (SS xs)               = null xs

fromList                      :: Ord a => [(a,b)] -> SS a b
fromList                      = SS . sortBy (relToFst (<))

toList                        :: Ord a => SS a b -> [(a,b)]
toList (SS xs)                = xs

lookup                        :: Ord a => SS a b -> a -> Maybe b
lookup (SS xs) k              = search xs
  where search []             = Nothing
        search (x@(k',v'):xs) = case compare k k' of
                                  EQ -> Just v'
                                  _ -> search xs

-- überschreibe Elemente mit gleichem Schlüssel
insert                        :: Ord a => SS a b -> (a,b) -> SS a b
insert ss p                   = insertBy compareKeys ss p

insertMany		      :: Ord a => SS a b -> [(a,b)] -> SS a b
insertMany ss ps	      = foldl insert ss ps

insertBy                      :: Ord a => OrderRel (a,b) ->  SS a b -> (a,b) -> SS a b
insertBy cmp (SS xs) p        = SS (insert xs)
  where insert []	      = [p]
        insert ys@(y:ys')     = case cmp p y of
				  GT -> y : insert ys'
				  EQ -> p : ys'
				  LT -> p : ys

-- keine Fehlermeldung, falls Element nicht vorhanden !
delete                        :: Ord a => SS a b -> a -> SS a b
delete ss@(SS xs) k           = deleteBy compare ss k

deleteMany		      :: Ord a => SS a b -> [a] -> SS a b
deleteMany ss ks	      = foldl delete ss ks

deleteBy		      :: Ord a => OrderRel a -> SS a b -> a -> SS a b
deleteBy cmp (SS xs) k	      = SS (delete xs)
  where delete []	      = []
	delete ys@(y:ys')     = case cmp k (key y) of
				  EQ -> delete ys'
				  _ -> y : delete ys'

-- predBy wird so in MonotonePartition benötigt
pred                          :: Ord a => SS a b -> a -> Maybe (a,b)
pred ss k                     = predBy compareKeys ss k

predBy		              :: Ord a => OrderRel (a,b) -> SS a b -> a -> Maybe (a,b)
predBy cmp (SS xs) k          = case filter (\ x -> cmp (k, undefined) x == GT) xs of
				  [] -> Nothing
				  ys -> Just (last ys)

{-
predBy		              :: Ord a => ((a,b) -> Ordering) -> SS a b -> a -> Maybe (a,b)
predBy cmp (SS xs) k          = case filter (\ x -> cmp x == LT) xs of
				  [] -> Nothing
				  ys -> Just (last ys)

pred                          :: Ord a => SS a b -> a -> Maybe (a,b)
pred ss k                     = predBy (\ x -> compare (key x) k) ss k
-}

succ                          :: Ord a => SS a b -> a -> Maybe (a,b)
succ ss k                     = succBy compare ss k

succBy		              :: Ord a => OrderRel a -> SS a b -> a -> Maybe (a,b)
succBy cmp (SS xs) k          = case filter (\ x -> cmp (key x) k == GT) xs of
				  [] -> Nothing
				  ys -> Just (Prelude.head ys)


swap                          :: Ord a => SS a b -> a -> a -> SS a b
swap ss@(SS xs) k l           = aux (StatusStructureSpec.lookup ss k) (StatusStructureSpec.lookup ss l)
  where aux Nothing Nothing   = error "wrong swap"
	aux Nothing (Just el)     = error "wrong swap"
	aux (Just ek) Nothing     = error "wrong swap"
	aux (Just ek) (Just el)   = SS (map exchange xs)
	  where exchange x@(m,_)
		  | m == k    = (l,el) 
		  | m == l    = (k,ek) 
		  | otherwise = x 
\end{code}

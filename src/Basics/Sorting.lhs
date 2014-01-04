%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
\subsection{Sortieren (|Sorting|)}
\module{Sorting}

> module Basics.Sorting (
>          nubSort, nubSortBy,
>          partition, partition3, partition3Sorted, merge, mergeBy, 
>          sort, sortBy, isort, isortBy, sortMap, sortMapBy,
>          nubSorted, nubSortedBy, 
>        ) where
> 
> import Basics.Utilities ( Rel, OrderRel, With (..), sat, liftToWith, lessRel, equalRel )
> import Basics.Sort	   ( sortLe )

|nubSort| und |nubSortBy|, der GHC-Bibliothek |Set| entnommen und an |Ordering| angepaßt.

> nubSort                       :: Ord a => [a] -> [a]
> nubSort                       = nubSortBy compare
> 
> nubSortBy                     :: OrderRel a -> [a] -> [a]
> {-
> nubSortBy cmp []		= []
> nubSortBy cmp [x]		= [x]
> nubSortBy cmp (x:xs)		= split x [] [] xs
>   where split x lo hi []      = nubSortBy cmp lo ++ (x : nubSortBy cmp hi)
>         split x lo hi (y:ys)  = case y `cmp` x of
>                                   LT -> split x (y:lo) hi     ys
>                                   EQ -> split x lo     hi     ys
>                                   GT -> split x lo     (y:hi) ys
> -}
> nubSortBy cmp                 = nubSortedBy (equalRel cmp) . sortBy (lessRel cmp) 

|nubSorted| löscht benachbarte äquivalente Elemente.

> nubSorted                     :: Eq a => [a] -> [a]
> nubSorted                     = nubSortedBy (==)
> 
> nubSortedBy                   :: Rel a -> [a] -> [a]
> nubSortedBy _ []              = []
> nubSortedBy _ [x]             = [x]
> nubSortedBy eq (x:y:ys) 
>   |  x `eq` y                 = nubSortedBy eq (y:ys) 
>   | otherwise                 = x : nubSortedBy eq (y:ys)

Während |sort| nur ein Wrapper ist, sind |sortMap| und |sortMapBy| mit dem
|Utilities.With| Datentyp implementiert.

> sort                          :: Ord a => [a] -> [a]
> sort                          = sortLe (<=)
> sortBy                        = sortLe
> 
> sortMap			:: Ord b => (a -> b) -> [a] -> [a]
> sortMap f xs			= map sat (sort [f x :& x | x<-xs])
> 
> sortMapBy			:: Rel b -> (a -> b) -> [a] -> [a]
> sortMapBy rel f xs		= map sat (sortBy (liftToWith rel) [f x :& x | x<-xs])

Einfaches Sortieren durch Einfügen.

> isort                         :: Ord a => [a] -> [a]
> isort                         = isortBy (<=)
> 
> isortBy                       :: Rel a -> [a] -> [a]
> isortBy leq                   = isort' []
>   where isort' ys []          = ys
>         isort' ys (x:xs)      = isort' (insert x ys) xs
>         insert k []           = [k]
>         insert k (x:xs)       = if k `leq` x then k:x:xs else x : insert k xs

Die Version |List.partition| aus der Prelude hat unter Hugs Schwierigkeiten mit großen Listen
aufgrund einer Beschränkung des Control-Stacks. Listen mit mehr als 700 Elementen können unter
Hugs nicht partitioniert werden, während folgende Version beliebig lange Listen erlaubt, aber
den Nachteil hat, nicht ``stabil'' zu sein.

> partition                     :: (a -> Bool) -> [a] -> ([a],[a])
> partition p xs                = part_aux xs [] []
>   where part_aux [] a b       = (a,b)  -- (reverse a, reverse b)
>         part_aux (x:xs) a b   = if p x then part_aux xs (x:a) b 
>                                 else part_aux xs a (x:b)

|partition3| ist die Verallgemeinerung auf |Ordering|.

> partition3                    :: (a -> Ordering) -> [a] -> ([a],[a],[a])
> partition3 p                  = foldr select ([],[],[])
>   where select x (ls,es,hs)   = case p x of 
>                                   LT -> (x:ls, es, hs) 
>                                   EQ -> (ls, x:es, hs)
>                                   GT -> (ls, es, x:hs) 
> 
> partition3Sorted	        :: (a -> Ordering) -> [a] -> ([a],[a],[a])
> partition3Sorted p xs         = part3aux xs [] []
>   where part3aux [] ls es     = (reverse ls, reverse es, [])
>         part3aux (x:xs) ls es = case p x of
> 				   LT -> part3aux xs (x:ls) es
> 				   EQ -> part3aux xs ls (x:es)
> 				   GT -> (reverse ls, reverse es, x:xs)

|merge| fügt zwei sortierte Listen zu einer sortierten Liste zusammen.

> merge                         :: Ord a => [a] -> [a] -> [a]
> merge                         = mergeBy (<=)
> 
> mergeBy                       :: Rel a -> [a] -> [a] -> [a]
> mergeBy leq [] []             = []
> mergeBy leq xs []             = xs
> mergeBy leq [] ys             = ys
> mergeBy leq (x:xs) (y:ys) 
>   | x `leq` y                 = x : mergeBy leq xs (y:ys)
>   | otherwise                 = y : mergeBy leq (x:xs) ys


%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Aufteilen von Punktmengen (|Divide|)}
\module{Divide}

> module Divide where
> 
> import Basics.Utilities ( OrderRel, Rel, equalRel )
> import Basics.Sorting   ( partition3, partition3Sorted )
> import Basics.OrderStat ( median, medianBy )
> import Basics.Sorting ( nubSortBy )
> 
> import List ( groupBy )
>
> type Partition a            = (a,([a],[a],[a]))
> 
> divideBy'		      :: (OrderRel a -> [a] -> a) -> OrderRel a -> [a] -> Partition a
> divideBy' median cmp xs     = (m, partition3 (flip cmp m) xs)
>   where m		      = median cmp xs
> 
> divideBy		      :: OrderRel a -> [a] -> Partition a
> divideBy		      = divideBy' medianBy
> 
> divide		      :: Ord a => [a] -> Partition a
> divide		      = divideBy compare
> 
> divideSortedBy	      :: OrderRel a -> [a] -> Partition a
> divideSortedBy cmp xs       = (m, partition3Sorted (flip cmp m) xs)
>   where m		      = xs !! (length xs `div` 2)
> 
> divideSorted		      :: Ord a => [a] -> Partition a
> divideSorted		      = divideSortedBy compare
> 
> data Split a                = Collinear' [a] | Split' [a] [a]
>			      deriving Show
>
> splitBy		      :: Num a => OrderRel a -> [a] -> Split a
> splitBy cmp		      = intoSplit . divideSortedBy cmp
>   where 
>   intoSplit (_, (ls, es, hs))     
>     | null ls && null hs    = Collinear' es
>     | null ls		      = Split' es hs
>     | null hs		      = Split' ls es
>     | otherwise	      = Split' ls (es ++ hs)
>
> data SplitTree a            = Point a 
>			      | Collinear [a] 
>			      | Split (SplitTree a) (SplitTree a)
>			      deriving Show
>
> splitTree                   :: OrderRel a -> OrderRel a -> [a] -> SplitTree a
> splitTree cmp1 cmp2 xs      = mkTree (map to grouped)
>   where sorted	      = nubSortBy cmp1 xs
>	  grouped	      = groupBy (equalRel cmp2) sorted
>	  to [x]	      = Point x
>	  to xs		      = Collinear xs
>	  mkTree [x]	      = x
>	  mkTree xs	      = Split (mkTree ls) (mkTree rs)
>	    where (ls, rs)    = splitAt (length xs `div` 2) xs
>
> instance Functor SplitTree where
>   fmap f (Point x)          = Point (f x)
>   fmap f (Collinear xs)     = Collinear (fmap f xs)
>   fmap f (Split ls rs)	  = Split (fmap f ls) (fmap f rs)
>
> reduce                      :: ([a] -> b) -> (b -> b -> b) -> SplitTree a -> b
> reduce f g (Point x)        = f [x]
> reduce f g (Collinear xs)   = f xs
> reduce f g (Split ls rs)    = g (reduce f g ls) (reduce f g rs)

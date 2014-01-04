%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Auswahl -- Order Statistics (|OrderStat|)}
\module{OrderStat}

> module Basics.OrderStat (
>         naiveSelect, naiveMedian, select, median,
>         naiveSelectBy, naiveMedianBy, selectBy, medianBy
>        ) where
> 
> import Basics.Sorting (sortBy, partition, isortBy, partition3)
> import Basics.Utilities (isSingleton, leqRel, longerThan, splitsAt)

> naiveSelect                   :: Ord a => Int -> [a] -> a
> naiveSelect			        = naiveSelectBy compare
> 
> naiveSelectBy                 :: (a -> a -> Ordering) -> Int -> [a] -> a
> naiveSelectBy cmp k xs        = sortBy (leqRel cmp) xs !! k
> 
> select           	            :: Ord a => Int -> [a] -> a
> select			            = selectBy compare
> 
> selectBy			:: (a -> a -> Ordering) -> Int -> [a] -> a
> selectBy cmp i xs
>   | xs `longerThan` 5         = if i < k then selectBy cmp i ls 
>                                 else if i < l then head es
>                                 else selectBy cmp (i-l) hs
>   | otherwise                 = sort xs !! i
>   where m                     = medianOfMedians xs
>	  (ls, es, hs)		= partition3 (flip cmp (medianOfMedians xs)) xs
>	  k			= length ls
>	  l			= k + length es
>	  leq			= leqRel cmp
>	  sort			= isortBy leq
>         medianOfMedians       = head . until isSingleton 
>				  (map (median5 . sort) . splitsAt 5)
>         median5 [_,_,x,_,_]   = x
>         median5 [_,x,y,_]     = if x `leq` y then x else y
>         median5 [_,x,_]       = x
>         median5 [x,y]         = if x `leq` y then x else y
>         median5 [x]           = x

Hiermit werden Medianfunktionen definiert.

> median, naiveMedian           :: Ord a => [a] -> a
> naiveMedian			        = naiveMedianBy compare
> median			            = medianBy compare
> 
> medianBy, naiveMedianBy       :: (a -> a -> Ordering) -> [a] -> a
> naiveMedianBy cmp xs          = naiveSelectBy cmp (length xs `div` 2) xs
> medianBy cmp xs               = selectBy cmp (length xs `div` 2) xs



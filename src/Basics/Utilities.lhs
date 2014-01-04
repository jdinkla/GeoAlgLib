%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Hilfsfunktionen (|Utilities|)}
\module{Utilities}

> module Basics.Utilities where
>
> import Data.List	 ( elemIndex )
> import Data.Maybe   ( fromJust )
>
> fst3                          :: (a,b,c) -> a
> fst3 (x,_,_)                  = x
> snd3                          :: (a,b,c) -> b
> snd3 (_,x,_)                  = x
> thd3                          :: (a,b,c) -> c
> thd3 (_,_,x)                  = x
> 
> fst4                          :: (a,b,c,d) -> a
> fst4 (x,_,_,_)                = x
> snd4                          :: (a,b,c,d) -> b
> snd4 (_,x,_,_)                = x
> thd4                          :: (a,b,c,d) -> c
> thd4 (_,_,x,_)                = x
> frt4                          :: (a,b,c,d) -> d
> frt4 (_,_,_,x)                  = x
> 
> curry3          	        :: ((a, b, c) -> d) -> (a -> b -> c -> d)
> curry3 f x y z                = f (x,y,z)
>
> uncurry3          	        :: (a -> b -> c -> d) -> ((a, b, c) -> d)
> uncurry3 f p                  = f (fst3 p) (snd3 p) (thd3 p)
> 

\subsubsection{Relationen}

> type Rel a		        = a -> a -> Bool
> type Rel3 a		        = a -> a -> a -> Bool
> 
> type OrderRel a		= a -> a -> Ordering
> type OrderRel3 a		= a -> a -> a -> Ordering
> 
> lessRel, leqRel, equalRel, geqRel, greaterRel :: OrderRel a -> Rel a
> lessRel cmp x y               = cmp x y == LT
> equalRel cmp x y              = cmp x y == EQ
> greaterRel cmp x y            = cmp x y == GT
> leqRel cmp x y                = not (cmp x y == GT)
> geqRel cmp x y                = not (cmp x y == LT)
> 
> reverseOrd			:: Ordering -> Ordering
> reverseOrd LT			= GT
> reverseOrd EQ			= EQ
> reverseOrd GT			= LT
> 
> compareEps                    :: (Ord a, Num a) => a -> a -> a -> Ordering
> compareEps eps x y		= if abs (x-y) < eps then EQ else compare x y
> 
> relToFst                      :: (a -> b -> c) -> (a,d) -> (b,e) -> c
> relToFst rel (x,_) (y,_)      = x `rel` y
> relToSnd                      :: (a -> b -> c) -> (d,a) -> (e,b) -> c
> relToSnd rel (_,x) (_,y)      = x `rel` y
> 
> choose1, choose2              :: Rel a -> (a -> a -> a)
> choose1 rel x y               = if x `rel` y then x else y
> choose2 rel x y               = if x `rel` y then y else x
>

\subsubsection{Minima und Maxima}

> minimumBy, maximumBy           :: Rel a -> [a] -> a
> minimumBy rel                 = foldl1 (\ x y -> if x `rel` y then x else y)
> -- minimumBy                      = foldl1 . choose1
> maximumBy                      = foldl1 . choose2
> 
> extremaBy		         :: Rel a -> [a] -> [a]
> extremaBy cmp []               = []
> extremaBy cmp [x]              = [x]
> extremaBy cmp xs@(_:_:_)       = [minimumBy cmp xs, maximumBy cmp xs]
> 
> data With a b                  = a :& b
> 
> sat                            :: With a b -> b
> sat (a :& b)                   = b
> 
> liftToWith                     :: (a -> b -> c) -> With a d -> With b e -> c
> liftToWith r (a :& _) (b :& _) = a `r` b
> 
> instance Eq a => Eq (With a b) where
>     (==)                       = liftToWith (==)
> 
> instance Ord a => Ord (With a b) where
>   compare                      = liftToWith compare
> 
> minimumWith, maximumWith       :: Ord b => (a -> b) -> [a] -> a
> minimumWith f xs		 = sat (minimum [f x :& x | x<-xs])
> maximumWith f xs		 = sat (maximum [f x :& x | x<-xs])
> 
> minima, maxima		 :: Ord a => [a] -> [a]
> minima			 = minimaBy compare
> maxima			 = maximaBy compare
> 
> minimaBy, maximaBy	         :: (a -> a -> Ordering) -> [a] -> [a]
> minimaBy cmp (x:xs)		 = foldl f [x] xs
>   where f ms@(h:_) b		 = case cmp b h of
> 			             LT -> [b]
> 				     EQ -> b:ms
> 				     GT -> ms			
> 
> maximaBy cmp (x:xs)	         = foldl f [x] xs
>   where f ms@(h:_) b	         = case cmp b h of
> 			             GT -> [b]
> 				     EQ -> b:ms
> 				     LT -> ms			
> 
> minimaWith, maximaWith         :: Ord b => (a -> b) -> [a] -> [a]
> minimaWith f xs		 = map sat (minima [f x :& x | x<-xs])
> maximaWith f xs		 = map sat (maxima [f x :& x | x<-xs])
> 
> minimaWithBy, maximaWithBy     :: (a -> a -> Ordering) -> (b -> a) -> [b] -> [b]
> minimaWithBy cmp f xs		 = map sat (minimaBy (liftToWith cmp) [f x :& x | x<-xs])
> maximaWithBy cmp f xs		 = map sat (maximaBy (liftToWith cmp) [f x :& x | x<-xs])

\subsubsection*{Listen}

> isSingleton [_]               = True
> isSingleton _                 = False
> 
> longerThan                    :: [a] -> Int -> Bool
> longerThan [] k               = k<0
> longerThan (x:xs) k           = k==0 || longerThan xs (k-1)
> 
> splitsAt                      :: Int -> [a] -> [[a]]
> splitsAt k []                 = []
> splitsAt k xs                 = ys : splitsAt k zs
>   where (ys, zs)              = splitAt k xs 
> 
> splitWhile			:: (a -> Bool) -> [a] -> ([a], [a])
> splitWhile p xs		= split xs []
>   where split [] ls		= (reverse ls, [])
> 	  split ys@(x:xs) ls	= if p x then split xs (x:ls) else (reverse ls, ys)
>
> sublist, sublist2, takeDrop   :: Eq a => a -> a -> [a] -> [a]
> sublist i j xs		= takeDrop i j xs ++ [j]
> sublist2 i j xs               = case takeDrop i j xs of { [] -> []; ys -> tail ys }
> takeDrop i j xs		= takeWhile (/=j) (dropWhile (/=i) (xs++xs))

|split| teilt ein Polygon in zwei Hälften,
|split x y [1..20] = ([1..x,y..20], [x..y])| wenn |x<=y|

> delete			:: Eq a => a -> a -> [a] -> [a]
> delete x y xs			= fst (split x y xs)
> 
> split                         :: Eq a => a -> a -> [a] -> ([a],[a])
> split x y xs			= splitByIndex (index x) (index y) xs 
>   where index x		= fromJust (elemIndex x xs)
> 
> splitByIndex			:: Int -> Int -> [a] -> ([a],[a])
> splitByIndex i j xs	      
>   | i <= j			= split i j
>   | otherwise			= split j i
>   where split i j             = (as ++ (xs!!i):(xs!!j):cs, bs)
>           where (as,rs)       = splitAt i xs
>                 (bs,cs)       = splitAt (j-i+1) rs
>
> rotateL, rotateR		:: [a] -> [a]
> rotateL xs			= tail xs ++ [head xs]
> rotateR xs			= [last xs] ++ init xs
> 
> rotate			:: Int -> [a] -> [a]
> rotate k xs			= iterate rotateL xs !! k
> 
> rotateTo			:: Eq a => a -> [a] -> [a]
> rotateTo x xs			= dropWhile (/= x) xs ++ takeWhile (/= x) xs

> rotateToBy			:: (a -> a -> Bool) -> a -> [a] -> [a]
> rotateToBy cmp x xs		= dropWhile rel xs ++ takeWhile rel xs
>   where rel z			= not (cmp z x)

\subsubsection*{Monaden}

> foreach :: Monad m => [a] -> (a -> m b) -> m [b]
> foreach xs p = mapM p xs
>
> foreach_ :: Monad m => [a] -> (a -> m ()) -> m ()
> foreach_ xs p = mapM_ p xs



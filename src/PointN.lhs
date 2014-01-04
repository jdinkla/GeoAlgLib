%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Mehrdimensionale Punkte (|PointN|)}
\module{PointN}

> module PointN (module PointClass, module PointN) where
> 
> import PointClass
> import Data.Array (Array, elems, listArray, bounds, (!))
> 
> newtype Num a => PointN a     = PointN (Array Int a) deriving Eq
> 
> instance (Show a, Num a) => Show (PointN a) where
>     showsPrec d (PointN xs)   = showsPrec d (elems xs)
> 
> pointN xs                     = PointN (listArray (1,length xs) xs)
> 
> instance Point PointN where
>     dimension (PointN a)      = snd (bounds a)
>     ith i (PointN a)          = a ! i
>     mapP f (PointN a)         = PointN (fmap f a)
>     origin                    = error "Point.PointN no origin"
>     (PointN x) <==> (PointN y)  
>       | bounds x == bounds y  = and (zipWith (==) (elems x) (elems y))
>                                 -- x == y mag Hugs nicht, obwohl Eq (PointN a) gilt
>       | otherwise             = error "Point.PointN dimension /="
>     (PointN x) <+> (PointN y) 
>       | bounds x == bounds y  = pointN (zipWith (+) (elems x) (elems y))
>       | otherwise             = error "Point.PointN dimension /="
>     (PointN x) <.> (PointN y) 
>       | bounds x ==bounds y   = sum (zipWith (*) (elems x) (elems y))
>       | otherwise             = error "Point.PointN dimension"
> 
> instance (Num a, Eq a)              => Num (PointN a) where
>     (+)			      = (<+>)
>     (-)			      = (<->)
>     negate		      = negateP
>     (*)			      = undefined
>     abs			      = mapP abs
>     signum		      = undefined
>     fromInteger		      = undefined


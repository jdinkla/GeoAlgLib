%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Erweiterung um maximales Element (|Topped|)}
\module{Topped}

a topped domain

> module Basics.Topped where
> 
> import Prelude hiding (RealFloat (isInfinite))
> 
> data Topped a                 = Finite a | Infty 
>                                 deriving (Eq, Ord)
> 
> instance Show a => Show (Topped a) where
>     showsPrec _ Infty         = showString "oo"
>     showsPrec _ (Finite x)    = shows x
> 
> instance Functor Topped where
>     fmap f (Finite a)         = Finite (f a)
>     fmap f Infty              = Infty
> 
> instance Monad Topped where
>     Infty >>= k               = Infty
>     Finite x >>= k            = k x
>     return                    = Finite
> 
> caseTopped                    :: (a -> b) -> b -> Topped a -> b
> caseTopped f i (Finite x)     = f x
> caseTopped f i Infty          = i
> 
> fromTopped                    :: Topped a -> a
> fromTopped (Finite x)         = x
> fromTopped Infty              = error "Topped.fromTopped Infty"
> 
> isInfty, isFinite             :: Topped a -> Bool
> isInfty                       = caseTopped (const False) True 
> isFinite                      = caseTopped (const True) False 
> 
>
> instance Enum a => Enum (Topped a) where
>     toEnum i 		        = Finite (toEnum i)
>     fromEnum (Finite x)	= fromEnum x
>     fromEnum Infty		= error "Topped.fromEnum Infty"
>     enumFrom (Finite x)	= map Finite (enumFrom x)
>     enumFrom Infty		= []
>     enumFromThen (Finite x) (Finite y) = map Finite (enumFromThen x y)
>     enumFromThen _ _	        = []

 instance Num a => Num (Topped a) where
   (Topped x) + (Topped y)     = Topped (x+y)
   _ + _                       = Infty
   (Topped x) * (Topped y)     = Topped (x*y)
   _ * _                       = Infty
   abs                         = map abs
   signum                      = map signum
   negate                      = map negate
   fromInteger                 = Topped . fromInteger
 

 instance Real a => Real (Topped a) where
     toRational (Topped x)     = toRational x
     toRational Infty		= error "Topped.toRational Infty"
                               
 instance Integral a => Integral (Topped a) where
     quotRem (Topped x) (Topped y) = (Topped a, Topped b) where (a,b) = quotRem x y
     quotRem _ _	        = error "Topped.quotRem Infty"

     toInteger (Topped x)      = toInteger x
     toInteger Infty		= error "Topped.toInteger Infty"
     toInt (Topped x)          = toInt x
     toInt Infty		= error "Topped.toInt Infty"

 instance Fractional a => Fractional (Topped a) where
     fromRational x            = Topped (fromRational x)
     (Topped x) / (Topped y)   = Topped (x/y)
     (Topped _) / Infty	= Topped 0
     Infty / (Topped _)	= Infty
     Infty / Infty		= Topped 1
 
 instance Floating a => Floating (Topped a) where
     pi			= Topped pi
     exp			= map exp
     log			= map log
     sin			= map sin
     cos			= map cos
     asin			= map asin
     acos			= map acos
     atan			= map atan
     sinh			= map sinh
     cosh			= map cosh
     asinh			= map asinh
     acosh			= map acosh
     atanh			= map atanh

 instance RealFrac a => RealFrac (Topped a) where
     properFraction (Topped x) = let (a,b) = properFraction x in (a, Topped b)
     properFraction Infty      = error "Topped.properFraction Infty"

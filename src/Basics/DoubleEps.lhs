%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Doubles mit $\varepsilon$-Umgebungen (|DoubleEps|)}
\module{DoubleEps}

> module Basics.DoubleEps ( DoubleEps (..), epsilon, toDouble ) where
> 
> newtype DoubleEps = DoubleEps Double deriving Read
>
> instance Show DoubleEps where
>    showsPrec k (DoubleEps x) = showsPrec k x

|DoubleEps| entspricht bis auf |==| Double. Gleichheit ist hier bezüglich einer $\varepsilon$
Umgebung definiert.

> epsilon                       :: Double
> epsilon			= 1.0e-5
>
> instance Eq DoubleEps where
>    (DoubleEps x) == (DoubleEps y) = abs (x-y) <= epsilon
>
> instance Ord DoubleEps where
>     compare (DoubleEps x) (DoubleEps y)
>       | abs diff <= epsilon = EQ
>       | diff < 0	      = LT
>       | otherwise	      = GT
>       where diff	      = x - y
> 
> toDouble (DoubleEps x)				= x
> destr1 f (DoubleEps x)				= f x
> destr2 f (DoubleEps x) (DoubleEps y)			= f x y
> destr3 f (DoubleEps x) (DoubleEps y) (DoubleEps z)	= f x y z
> lift1 f (DoubleEps x)					= DoubleEps (f x)
> lift2 f (DoubleEps x) (DoubleEps y)			= DoubleEps (f x y)
> 
> instance Num DoubleEps where
>    (+)		      = lift2 (+)
>    (*)		      = lift2 (*)
>    (-)		      = lift2 (-)
>    negate		      = lift1 negate
>    abs		      = lift1 abs
>    signum		      = lift1 signum
>    fromInteger	  = DoubleEps . fromInteger
> 
> 
> instance Real DoubleEps where
>    toRational		      = toRational . toDouble
> 
> 
> instance Fractional DoubleEps where
>    (/)		      = lift2 (/)
>    recip		      = lift1 recip
>    fromRational	      = DoubleEps . fromRational
> 
> instance Floating DoubleEps where
>     pi	              = DoubleEps pi
>     exp		      = lift1 exp
>     log		      = lift1 log
>     sqrt		      = lift1 sqrt
>     (**)		      = lift2 (**)
>     logBase		      = lift2 logBase
>     sin		      = lift1 sin
>     cos		      = lift1 cos
>     tan		      = lift1 tan
>     asin		      = lift1 asin
>     acos		      = lift1 acos
>     atan		      = lift1 atan
>     sinh		      = lift1 sinh
>     cosh		      = lift1 cosh
>     tanh		      = lift1 tanh
>     asinh		      = lift1 asinh
>     acosh		      = lift1 acosh
>     atanh		      = lift1 atanh
> 
> 
> instance RealFrac DoubleEps where
>     properFraction	      = (\ (x,y) -> (x, DoubleEps y)) . destr1 properFraction
>     truncate		      = destr1 truncate
>     round		      = destr1 round
>     ceiling		      = destr1 ceiling
>     floor		      = destr1 floor
> 
> 
> instance RealFloat DoubleEps where
>     floatRadix	      = destr1 floatRadix
>     floatDigits	      = destr1 floatDigits
>     floatRange	      = destr1 floatRange
>     decodeFloat	      = destr1 decodeFloat
>     encodeFloat i	      = DoubleEps . encodeFloat i
>     exponent		      = destr1 exponent
>     significand	      = lift1 significand
>     scaleFloat i	      = lift1 (scaleFloat i)
>     isNaN		      = destr1 isNaN
>     isInfinite	      = destr1 isInfinite
>     isDenormalized	      = destr1 isDenormalized
>     isNegativeZero	      = destr1 isNegativeZero
>     isIEEE		      = destr1 isIEEE
> 
> 
> instance Enum DoubleEps where
>     toEnum		      = DoubleEps . toEnum
>     fromEnum		      = destr1 fromEnum
>     enumFrom		      = map DoubleEps . destr1 enumFrom	 
>     enumFromThen x y	      = map DoubleEps (destr2 enumFromThen x y)
>     enumFromTo x y	      = map DoubleEps (destr2 enumFromTo x y)
>     enumFromThenTo x y z    = map DoubleEps (destr3 enumFromThenTo x y z)

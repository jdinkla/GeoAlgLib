%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsubsection{Die Typklasse \texttt{Point} (|PointClass|)}
\module{PointClass}

\begin{code}
module PointClass ( 
         Point (..), toList,
	 xcoord, ycoord, zcoord, xDistance, yDistance, zDistance,
	 lexic, 
	 lessX, leqX, equalX, geqX, greaterX, 
	 lessY, leqY, equalY, geqY, greaterY,
	 lessZ, leqZ, equalZ, geqZ, greaterZ,
	 lessIth, leqIth, equalIth, geqIth, greaterIth,
	 compareIthBy, compareIth, compareX, compareY, compareZ,
	 inIntervalIth, inInterval, inIntervalX, inIntervalY, inIntervalZ,
	 norm, distance, sqrDistance
       ) 
where

import Basics.Utilities (Rel, OrderRel)

infixl 7 <.>
infixl 6 <*>
infixl 5 <+>, <->
infix  4 <==>, </=>
\end{code}

\begin{code}
class Point p where
    dimension                 :: (Num a, Eq a) => p a -> Int
    ith                       :: (Num a, Eq a) => Int -> p a -> a
    origin                    :: (Num a, Eq a) => p a

    -- Functor
    mapP                      :: (Num a, Num b, Eq a, Eq b) => (a -> b) -> p a -> p b 

    -- Eq    
    (<==>)                    :: (Num a, Eq a) => p a -> p a -> Bool
    (</=>)                    :: (Num a, Eq a) => p a -> p a -> Bool

    -- Num
    (<+>)                     :: (Num a, Eq a) => p a -> p a -> p a
    (<->)                     :: (Num a, Eq a) => p a -> p a -> p a
    negateP                   :: (Num a, Eq a) => p a -> p a

    (<.>)                     :: (Num a, Eq a) => p a -> p a -> a
    (<*>)                     :: (Num a, Eq a) => a -> p a -> p a

    x </=> y                  = not (x <==> y)
    negateP                   = mapP (0-)
    v <-> w                   = v <+> negateP w
    k <*> p                   = mapP (k*) p
\end{code}

\begin{code}
toList                        :: (Point p, Num a, Eq a) => p a -> [a]
toList p                      = [ ith i p | i <- [1..dimension p]]

xcoord, ycoord, zcoord        :: (Num a, Eq a, Point p) => p a -> a
xcoord                        = ith 1
ycoord                        = ith 2
zcoord                        = ith 3

xDistance, yDistance, 
  zDistance                   :: (Num a, Eq a, Point p) => p a -> p a -> a
xDistance p q                 = abs (xcoord p - xcoord q)
yDistance p q                 = abs (ycoord p - ycoord q)
zDistance p q                 = abs (zcoord p - zcoord q)
\end{code}

Boolesche und \texttt{Ordering}-Relationen.

Ordnungen auf Punkten
\begin{code}
lexic                         :: (Point p, Num a, Eq a, Ord a) => OrderRel (p a)
p `lexic` q                   = case is of
                                  [] -> EQ
                                  (i:_) -> compareIth i p q
  where is                    = dropWhile (\ i -> equalIth i p q) [1..dimension p]
\end{code}

Vergleiche einzelner Dimensionen

\begin{code}
lessIth, leqIth, equalIth,
  geqIth, greaterIth        :: (Point p, Ord a, Eq a, Num a) => Int -> Rel (p a)

lessIth			      = cmpAux (<)
leqIth			      = cmpAux (<=)
equalIth		      = cmpAux (==)
geqIth		              = cmpAux (>=)
greaterIth		      = cmpAux (>)

cmpAux                        :: (Point p, Num a, Eq a) => Rel a -> Int -> Rel (p a)
cmpAux p i x y                = ith i x `p` ith i y

lessX, leqX, equalX, geqX, greaterX,
    lessY, leqY, equalY, geqY, greaterY,
    lessZ, leqZ, equalZ, geqZ, greaterZ :: (Point p, Ord a, Num a, Eq a) => Rel (p a)
lessX			      = lessIth 1
lessY			      = lessIth 2
lessZ			      = lessIth 3
leqX			      = leqIth 1
leqY			      = leqIth 2
leqZ			      = leqIth 3
equalX			      = equalIth 1
equalY			      = equalIth 2
equalZ			      = equalIth 3
geqX			      = geqIth 1
geqY			      = geqIth 2
geqZ			      = geqIth 3
greaterX		      = greaterIth 1
greaterY		      = greaterIth 2
greaterZ		      = greaterIth 3

compareIthBy                  :: (Num a, Eq a, Point p) => OrderRel a -> Int -> OrderRel (p a)
compareIthBy cmp i x y        = cmp (ith i x) (ith i y)

compareIth                    :: (Ord a, Num a, Eq a, Point p) => Int -> OrderRel (p a)
compareIth		      = compareIthBy compare

compareX, compareY, compareZ  :: (Ord a, Num a, Eq a, Point p) => OrderRel (p a)
compareX		      = compareIth 1
compareY		      = compareIth 2
compareZ		      = compareIth 3
\end{code}

Tests auf Enthaltensein in einem Intervall

\begin{code}
inIntervalIth                 :: (Point p, Num a, Ord a, Eq a) => Int -> p a -> (p a,p a) -> Bool
inIntervalIth i x (p,q)       = ith i p <= y && y <= ith i q
  where y                     = ith i x

inIntervalX, inIntervalY,
    inIntervalZ		      :: (Point p, Num a, Ord a, Eq a) => p a -> (p a, p a) -> Bool
inIntervalX		      = inIntervalIth 1
inIntervalY		      = inIntervalIth 2
inIntervalZ		      = inIntervalIth 3

inInterval                    :: (Point p, Num a, Ord a, Eq a) => p a -> (p a,p a) -> Bool
x `inInterval` (p,q)          = all (\ i -> inIntervalIth i x (p,q)) [1..dimension x]
\end{code}

\begin{code}
norm                          :: (Point p, Floating a, Eq a) => p a -> a
norm x                        = sqrt (x <.> x)

distance                      :: (Point p, Floating a, Eq a) => p a -> p a -> a
distance p q                  = norm (p <-> q)

sqrDistance                   :: (Point p, Num a, Eq a) => p a -> p a -> a
sqrDistance p q               = r <.> r where r = p <-> q
\end{code}


%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% jdi 10.05.1997,  4.09.1997
%
\subsection{Listen mit ``assoziativer'' Konkatenation (|ListCat|)}
\module{ListCat}

\cite[K. 4]{hughes96:pretty}

> module Basics.ListCat where
> 
> type ListCat a                 = [a] -> [a]
> 
> nil                            :: ListCat a
> nil xs                         = xs
> 
> unit                           :: a -> ListCat a
> unit x zs                      = x : zs
> 
> cat                            :: ListCat a -> ListCat a -> ListCat a
> cat xs ys zs                   = xs (ys zs)
> 
> cats				 :: [ListCat a] -> ListCat a
> cats                         	 = foldr cat nil
> 
> list                           :: ListCat a -> [a]
> list xs                        = xs []
> 
> toListCat		         :: [a] -> ListCat a
> toListCat		         = foldr (cat . unit) nil


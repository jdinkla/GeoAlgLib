%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Ereignisstruktur |EventStructureSpec|}
\module{EventStructureSpec}

\begin{code}
module EventStructureSpec where

import Data.List (insertBy)
import qualified Data.List (delete)
import Basics.Sorting (sort)

data Event t a                = Event { time :: t, event :: a } deriving Show

instance (Eq t)	=> Eq (Event t a) where
    x == y                    = time x == time y

instance (Ord t) => Ord (Event t a) where
    compare p q               = compare (time p) (time q)

newtype ES t a                = ES [Event t a]

empty                         :: ES t a
empty                         = ES []

isEmpty                       :: ES t a -> Bool
isEmpty (ES xs)               = null xs

fromList                      :: Ord t => [Event t a] -> ES t a
fromList xs                   = ES (sort xs)

toList                        :: Ord t => ES t a  -> [Event t a]
toList (ES xs)                = xs

size                          :: ES t a -> Int
size (ES xs)                  = length xs

insert                        :: Ord t => ES t a -> Event t a -> ES t a
insert (ES xs) e	          = ES (insertBy compare e xs)

insertMany		              :: Ord t => ES t a -> [Event t a] -> ES t a
insertMany       	          = foldl insert 

insertC                       :: Ord t => (a -> a -> a) -> ES t a -> Event t a -> ES t a
insertC f (ES xs) e	          = ES (insert xs)
  where 
    insert []                 = [e]
    insert evs@(e':es)        = case compare e' e of
                                  	LT -> e' : insert es
                                  	EQ -> Event (time e) (f (event e') (event e)) : es
                                  	GT -> e : evs

insertManyC                   :: Ord t => (a -> a -> a) -> ES t a -> [Event t a] -> ES t a
insertManyC f                 = foldl (insertC f)

delete                        :: Ord t => ES t a -> t -> ES t a
delete (ES xs) t	          = ES (Data.List.delete (Event t undefined) xs)

deleteMany		              :: Ord t => ES t a -> [t] -> ES t a
deleteMany            	      = foldl delete

head                          :: ES t a -> (Event t a)
head (ES [])                  = error "ES.head []"
head (ES xs)                  = Prelude.head xs

tail                          :: ES t a -> ES t a
tail (ES [])                  = error "ES.tail []"
tail (ES xs)                  = ES (Prelude.tail xs)
\end{code}

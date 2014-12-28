--------------------------------------------------------------------------------
-- Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
--------------------------------------------------------------------------------
--
--  9.11.1997
\subsection{Endliche Abbildungen (\texttt{FiniteMap})}
\module{FiniteMap}

Dieses ist das Modul \texttt{FiniteMap} der GHC-Bibliothek. Wir haben
nur die Funktionen umbenannt.

Die von mir hinzugefügten Funktionen \texttt{prev}, \texttt{prevBy},
\texttt{next}, \texttt{nextBy}, \texttt{addToBy} und
\texttt{addToBy_C} sind fehlerhaft. Ich hatte keine Zeit sie genauer
zu implementieren.

--
-- (c) The AQUA Project, Glasgow University, 1994-1996
--
\section[FiniteMap]{An implementation of finite maps}

``Finite maps'' are the heart of the compiler's
lookup-tables/environments and its implementation of sets.  Important
stuff!

This code is derived from that in the paper:
\begin{display}
	S Adams
	"Efficient sets: a balancing act"
	Journal of functional programming 3(4) Oct 1993, pp553-562
\end{display}

\begin{code}
module Basics.FiniteMap (
	FiniteMap,		-- abstract type
	empty, unit, fromList,
	insert, insertC, insertMany, insertManyC, insertBy, insertByC,
	delete, deleteMany, deleteBy, deleteManyBy,
	plus, plus_C, minus, fold,
	intersect, intersect_C , map, filter,
	size, isEmpty, elem, lookup, lookupWithDefault,
	toList, keys, elts,
	pred, succ, predBy, succBy, swap
    ) where

import Prelude hiding (elem, lookup, map, filter, succ, pred)
import Maybes
\end{code}


--************************************************************************
--*									*
\subsection{The signature of the module}
--*									*
--************************************************************************

\begin{code}
--	BUILDING
empty		:: FiniteMap key elt
unit		:: (key, elt) -> FiniteMap key elt
fromList	:: (Ord key) => [(key,elt)] -> FiniteMap key elt
			-- In the case of duplicates, the last is taken

--	ADDING AND DELETING
		   -- Throws away any previous binding
		   -- In the list case, the items are added starting with the
		   -- first one in the list
insert		:: (Ord key) => FiniteMap key elt -> (key, elt)  -> FiniteMap key elt
insertMany	:: (Ord key) => FiniteMap key elt -> [(key,elt)] -> FiniteMap key elt

		   -- Combines with previous binding
		   -- In the combining function, the first argument is the "old" element,
		   -- while the second is the "new" one.
insertC		:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> (key, elt)
			   -> FiniteMap key elt
insertManyC	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> [(key,elt)]
			   -> FiniteMap key elt

-- jdi 12.11.1997
insertBy  	:: (Ord key) => ((key, elt) -> (key, elt) -> Ordering)
			   -> FiniteMap key elt -> (key, elt)
			   -> FiniteMap key elt

-- jdi 12.11.1997
insertByC	:: (Ord key) => ((key, elt) -> (key, elt) -> Ordering)
			   -> (elt -> elt -> elt)
			   -> FiniteMap key elt -> (key, elt)
			   -> FiniteMap key elt


		   -- Deletion doesn't complain if you try to delete something
		   -- which isn't there
delete		:: (Ord key) => FiniteMap key elt -> key   -> FiniteMap key elt
deleteMany	:: (Ord key) => FiniteMap key elt -> [key] -> FiniteMap key elt
deleteBy	:: (Ord key) => ((key, elt) -> (key, elt) -> Ordering) -> FiniteMap key elt
		-> (key,elt)   -> FiniteMap key elt
deleteManyBy	:: (Ord key) => ((key, elt) -> (key, elt) -> Ordering) -> FiniteMap key elt
		-> [(key,elt)] -> FiniteMap key elt

--	COMBINING
		   -- Bindings in right argument shadow those in the left
plus		:: (Ord key) => FiniteMap key elt -> FiniteMap key elt
			   -> FiniteMap key elt

		   -- Combines bindings for the same thing with the given function
plus_C		:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

minus		:: (Ord key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
		   -- (minus a1 a2) deletes from a1 any bindings which are bound in a2

intersect	:: (Ord key) => FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt
intersect_C	:: (Ord key) => (elt -> elt -> elt)
			   -> FiniteMap key elt -> FiniteMap key elt -> FiniteMap key elt

--	MAPPING, FOLDING, FILTERING
fold		:: (key -> elt -> a -> a) -> a -> FiniteMap key elt -> a
map		:: (key -> elt1 -> elt2) -> FiniteMap key elt1 -> FiniteMap key elt2
filter		:: (Ord key) => (key -> elt -> Bool)
			   -> FiniteMap key elt -> FiniteMap key elt

--	INTERROGATING
size		:: FiniteMap key elt -> Int
isEmpty		:: FiniteMap key elt -> Bool

elem		:: (Ord key) => key -> FiniteMap key elt -> Bool
lookup		:: (Ord key) => FiniteMap key elt -> key -> Maybe elt
lookupWithDefault
		:: (Ord key) => FiniteMap key elt -> elt -> key -> elt
		-- lookupWithDefault supplies a "default" elt
		-- to return for an unmapped key

--	LISTIFYING
toList		:: FiniteMap key elt -> [(key,elt)]
keys		:: FiniteMap key elt -> [key]
elts		:: FiniteMap key elt -> [elt]
\end{code}

--************************************************************************
--*									*
\subsection{The @FiniteMap@ data type, and building of same}
--*									*
--************************************************************************

Invariants about @FiniteMap@:
\begin{enumerate}
\item
all keys in a FiniteMap are distinct
\item
all keys in left  subtree are $<$ key in Branch and
all keys in right subtree are $>$ key in Branch
\item
size field of a Branch gives number of Branch nodes in the tree
\item
size of left subtree is differs from size of right subtree by a
factor of at most \tr{sIZE_RATIO}
\end{enumerate}

\begin{code}
data FiniteMap key elt
  = EmptyFM
  | Branch key elt	    	-- Key and elt stored here
    !Int   		        -- Size >= 1
    (FiniteMap key elt)	    	-- Children
    (FiniteMap key elt)
\end{code}

\begin{code}
empty = EmptyFM
{-
emptyFM
  = Branch bottom bottom IF_GHC(0#,0) bottom bottom
  where
    bottom = panic "emptyFM"
-}

-- #define EmptyFM (Branch _ _ IF_GHC(0#,0) _ _)

unit (key, elt) = Branch key elt 1 empty empty

fromList = insertMany empty
\end{code}

--************************************************************************
--*									*
\subsection{Adding to and deleting from @FiniteMaps@}
--*									*
--************************************************************************

\begin{code}
insert fm x = insertC (\ old new -> new) fm x

insertC combiner EmptyFM x@(key, elt) = unit x
insertC combiner (Branch key elt size fm_l fm_r) x@(new_key, new_elt)
  | new_key < key = mkBalBranch key elt (insertC combiner fm_l x) fm_r
  | new_key > key = mkBalBranch key elt fm_l (insertC combiner fm_r x)
  | otherwise	  = Branch new_key (combiner elt new_elt) size fm_l fm_r

-- jdi 12.11.1997
insertBy cmp fm x = insertByC cmp (\ old new -> new) fm x

-- jdi 12.11.1997
insertByC compare combiner EmptyFM x = unit x
insertByC compare combiner (Branch key elt size fm_l fm_r) x@(new_key, new_elt)
  = case x `compare` (key,elt) of
      LT -> mkBalBranch key elt (insertByC compare combiner fm_l x) fm_r
      GT -> mkBalBranch key elt fm_l (insertByC compare combiner fm_r x)
      EQ -> Branch new_key (combiner elt new_elt) size fm_l fm_r

insertMany fm key_elt_pairs = insertManyC (\ old new -> new) fm key_elt_pairs

insertManyC combiner fm key_elt_pairs
  = foldl add fm key_elt_pairs	-- foldl adds from the left
  where
    add fmap x = insertC combiner fmap x
\end{code}

\begin{code}
delete EmptyFM del_key = empty
delete (Branch key elt size fm_l fm_r) del_key
  | del_key > key
  = mkBalBranch key elt fm_l (delete fm_r del_key)

  | del_key < key
  = mkBalBranch key elt (delete fm_l del_key) fm_r

  | key == del_key
  = glueBal fm_l fm_r

deleteMany = foldl delete

deleteBy cmp EmptyFM e = empty
deleteBy cmp (Branch key elt size fm_l fm_r) e
  = case cmp e (key, elt) of
      LT -> mkBalBranch key elt (deleteBy cmp fm_l e) fm_r
      EQ -> glueBal fm_l fm_r
      GT -> mkBalBranch key elt fm_l (deleteBy cmp fm_r e)

deleteManyBy cmp = foldl (deleteBy cmp)
\end{code}

--************************************************************************
--*									*
\subsection{Combining @FiniteMaps@}
--*									*
--************************************************************************

\begin{code}
plus_C combiner EmptyFM fm2 = fm2
plus_C combiner fm1 EmptyFM = fm1
plus_C combiner fm1 (Branch split_key elt2 _ left right)
  = mkVBalBranch split_key new_elt
		 (plus_C combiner lts left)
		 (plus_C combiner gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key
    new_elt = case lookup fm1 split_key of
		Nothing   -> elt2
		Just elt1 -> combiner elt1 elt2

-- It's worth doing plusFM specially, because we don't need
-- to do the lookup in fm1.

plus EmptyFM fm2 = fm2
plus fm1 EmptyFM = fm1
plus fm1 (Branch split_key elt1 _ left right)
  = mkVBalBranch split_key elt1 (plus lts left) (plus gts right)
  where
    lts     = splitLT fm1 split_key
    gts     = splitGT fm1 split_key

minus EmptyFM fm2 = empty
minus fm1 EmptyFM = fm1
minus fm1 (Branch split_key elt _ left right)
  = glueVBal (minus lts left) (minus gts right)
	-- The two can be way different, so we need glueVBal
  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

intersect fm1 fm2 = intersect_C (\ left right -> right) fm1 fm2

intersect_C combiner fm1 EmptyFM = empty
intersect_C combiner EmptyFM fm2 = empty
intersect_C combiner fm1 (Branch split_key elt2 _ left right)

  | maybeToBool maybe_elt1	-- split_elt *is* in intersection
  = mkVBalBranch split_key (combiner elt1 elt2) (intersect_C combiner lts left)
						(intersect_C combiner gts right)

  | otherwise			-- split_elt is *not* in intersection
  = glueVBal (intersect_C combiner lts left) (intersect_C combiner gts right)

  where
    lts = splitLT fm1 split_key		-- NB gt and lt, so the equal ones
    gts = splitGT fm1 split_key		-- are not in either.

    maybe_elt1 = lookup fm1 split_key
    Just elt1  = maybe_elt1
\end{code}

--************************************************************************
--*									*
\subsection{Mapping, folding, and filtering with @FiniteMaps@}
--*									*
--************************************************************************

\begin{code}
fold k z EmptyFM = z
fold k z (Branch key elt _ fm_l fm_r)
  = fold k (k key elt (fold k z fm_r)) fm_l

map f EmptyFM = empty
map f (Branch key elt size fm_l fm_r)
  = Branch key (f key elt) size (map f fm_l) (map f fm_r)

filter p EmptyFM = empty
filter p (Branch key elt _ fm_l fm_r)
  | p key elt		-- Keep the item
  = mkVBalBranch key elt (filter p fm_l) (filter p fm_r)

  | otherwise		-- Drop the item
  = glueVBal (filter p fm_l) (filter p fm_r)
\end{code}

--************************************************************************
--*									*
\subsection{Interrogating @FiniteMaps@}
--*									*
--************************************************************************

\begin{code}
--{-# INLINE sizeFM #-}
size EmptyFM		     = 0
size (Branch _ _ size _ _) = size

isEmpty fm = size fm == 0

--
-- begin jdi 12.11.1997
--

type OrderRel a = a -> a -> Ordering

searchBy :: Ord key => OrderRel (key, elt) -> (FiniteMap key elt -> Maybe a)
	 -> FiniteMap key elt -> key -> Maybe a

searchBy _ _ EmptyFM _ = Nothing
searchBy cmp f b@(Branch key elt _ fm_l fm_r) key_to_find
  = case (key_to_find, undefined) `cmp` (key, elt) of
      LT -> searchBy cmp f fm_l key_to_find
      GT -> searchBy cmp f fm_r key_to_find
      EQ -> f b

search :: Ord key => (FiniteMap key elt -> Maybe a) -> FiniteMap key elt -> key -> Maybe a
search = searchBy (\ (k,_) (l, _) -> compare k l)

lookup = search (\ (Branch key elt _ fm_l fm_r) -> Just elt)

predBy, succBy :: Ord key => OrderRel (key,elt) -> FiniteMap key elt -> key -> Maybe (key, elt)
predBy cmp = searchBy cmp (\ (Branch key elt _ fm_l fm_r) -> maxNode fm_l)
succBy cmp = searchBy cmp (\ (Branch key elt _ fm_l fm_r) -> maxNode fm_l)

pred, succ :: Ord key => FiniteMap key elt -> key -> Maybe (key, elt)
pred   = search (\ (Branch key elt _ fm_l fm_r) -> maxNode fm_l)
succ   = search (\ (Branch key elt _ fm_l fm_r) -> minNode fm_r)

minNode, maxNode :: Ord key => FiniteMap key elt -> Maybe (key, elt)
maxNode EmptyFM = Nothing
maxNode (Branch key elt _ fm_l EmptyFM) = Just (key, elt)
maxNode (Branch key elt _ fm_l fm_r)    = maxNode fm_r

minNode EmptyFM = Nothing
minNode (Branch key elt _ EmptyFM fm_r) = Just (key, elt)
minNode (Branch key elt _ fm_l fm_r)    = minNode fm_l

swap :: Ord key => FiniteMap key elt -> key -> key -> FiniteMap key elt
swap fm k l           = aux (lookup fm k) (lookup fm l)
  where aux Nothing Nothing   = error "wrong swap"
	aux Nothing (Just el) = error "wrong swap"
	aux (Just ek) Nothing = error "wrong swap"
	aux (Just ek) (Just el) = exchange fm (k,ek) (l,el)
	exchange EmptyFM _ _ = EmptyFM
	exchange (Branch k e s ls rs) x@(l,f) y@(m,g)
	  | k==l      = Branch m g s (exchange ls x y) (exchange rs x y)
	  | k==m      = Branch l f s (exchange ls x y) (exchange rs x y)
	  | otherwise = Branch k e s (exchange ls x y) (exchange rs x y)

{-
lookup EmptyFM key = Nothing
lookup (Branch key elt _ fm_l fm_r) key_to_find
  | key_to_find < key = lookup fm_l key_to_find
  | key_to_find > key = lookup fm_r key_to_find
  | otherwise	  = Just elt
-}

--
-- end jdi 12.11.1997
--

key `elem` fm
  = case (lookup fm key) of { Nothing -> False; Just elt -> True }

lookupWithDefault fm deflt key
  = case (lookup fm key) of { Nothing -> deflt; Just elt -> elt }
\end{code}

--************************************************************************
--*									*
\subsection{Listifying @FiniteMaps@}
--*									*
--************************************************************************

\begin{code}
toList fm = fold (\ key elt rest -> (key,elt) : rest) [] fm
keys fm   = fold (\ key elt rest -> key : rest)       [] fm
elts fm   = fold (\ key elt rest -> elt : rest)       [] fm
\end{code}


--************************************************************************
--*									*
\subsection{The implementation of balancing}
--*									*
--************************************************************************

--************************************************************************
--*									*
\subsubsection{Basic construction of a @FiniteMap@}
--*									*
--************************************************************************

@mkBranch@ simply gets the size component right.  This is the ONLY
(non-trivial) place the Branch object is built, so the ASSERTion
recursively checks consistency.  (The trivial use of Branch is in
@unitFM@.)

\begin{code}
sIZE_RATIO :: Int
sIZE_RATIO = 5

mkBranch :: (Ord key) 		-- Used for the assertion checking only
	 => Int
	 -> key -> elt
	 -> FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

mkBranch which key elt fm_l fm_r
  = --ASSERT( left_ok && right_ok && balance_ok )
    let
	result = Branch key elt (unbox (1 + left_size + right_size)) fm_l fm_r
    in
--    if sizeFM result <= 8 then
	result
--    else
--	pprTrace ("mkBranch:"++(show which)) (ppr PprDebug result) (
--	result
--	)
  where
    left_ok  = case fm_l of
		EmptyFM		         -> True
		Branch left_key _ _ _ _  -> let
						biggest_left_key = fst (findMax fm_l)
					    in
					    biggest_left_key < key
    right_ok = case fm_r of
		EmptyFM		         -> True
		Branch right_key _ _ _ _ -> let
						smallest_right_key = fst (findMin fm_r)
					    in
					    key < smallest_right_key
    balance_ok = True -- sigh
{- LATER:
    balance_ok
      = -- Both subtrees have one or no elements...
	(left_size + right_size <= 1)
-- NO	      || left_size == 0  -- ???
-- NO	      || right_size == 0 -- ???
    	-- ... or the number of elements in a subtree does not exceed
	-- sIZE_RATIO times the number of elements in the other subtree
      || (left_size  * sIZE_RATIO >= right_size &&
    	  right_size * sIZE_RATIO >= left_size)
-}

    left_size  = size fm_l
    right_size = size fm_r

    unbox :: Int -> Int
    unbox x = x
\end{code}

--************************************************************************
--*									*
\subsubsection{{\em Balanced} construction of a @FiniteMap@}
--*									*
--************************************************************************

@mkBalBranch@ rebalances, assuming that the subtrees aren't too far
out of whack.

\begin{code}
mkBalBranch :: (Ord key)
	    => key -> elt
	    -> FiniteMap key elt -> FiniteMap key elt
	    -> FiniteMap key elt

mkBalBranch key elt fm_L fm_R

  | size_l + size_r < 2
  = mkBranch 1{-which-} key elt fm_L fm_R

  | size_r > sIZE_RATIO * size_l	-- Right tree too big
  = case fm_R of
	Branch _ _ _ fm_rl fm_rr
		| size fm_rl < 2 * size fm_rr -> single_L fm_L fm_R
		| otherwise	   	          -> double_L fm_L fm_R
	-- Other case impossible

  | size_l > sIZE_RATIO * size_r	-- Left tree too big
  = case fm_L of
	Branch _ _ _ fm_ll fm_lr
		| size fm_lr < 2 * size fm_ll -> single_R fm_L fm_R
		| otherwise		          -> double_R fm_L fm_R
	-- Other case impossible

  | otherwise				-- No imbalance
  = mkBranch 2{-which-} key elt fm_L fm_R

  where
    size_l   = size fm_L
    size_r   = size fm_R

    single_L fm_l (Branch key_r elt_r _ fm_rl fm_rr)
	= mkBranch 3{-which-} key_r elt_r (mkBranch 4{-which-} key elt fm_l fm_rl) fm_rr

    double_L fm_l (Branch key_r elt_r _ (Branch key_rl elt_rl _ fm_rll fm_rlr) fm_rr)
	= mkBranch 5{-which-} key_rl elt_rl (mkBranch 6{-which-} key   elt   fm_l   fm_rll)
				 (mkBranch 7{-which-} key_r elt_r fm_rlr fm_rr)

    single_R (Branch key_l elt_l _ fm_ll fm_lr) fm_r
	= mkBranch 8{-which-} key_l elt_l fm_ll (mkBranch 9{-which-} key elt fm_lr fm_r)

    double_R (Branch key_l elt_l _ fm_ll (Branch key_lr elt_lr _ fm_lrl fm_lrr)) fm_r
	= mkBranch 10{-which-} key_lr elt_lr (mkBranch 11{-which-} key_l elt_l fm_ll  fm_lrl)
				 (mkBranch 12{-which-} key   elt   fm_lrr fm_r)
\end{code}


\begin{code}
mkVBalBranch :: (Ord key)
	     => key -> elt
	     -> FiniteMap key elt -> FiniteMap key elt
	     -> FiniteMap key elt

-- Assert: in any call to (mkVBalBranch_C comb key elt l r),
--	   (a) all keys in l are < all keys in r
--	   (b) all keys in l are < key
--	   (c) all keys in r are > key

mkVBalBranch key elt EmptyFM fm_r = insert fm_r (key, elt)
mkVBalBranch key elt fm_l EmptyFM = insert fm_l (key, elt)

mkVBalBranch key elt fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
		     fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (mkVBalBranch key elt fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (mkVBalBranch key elt fm_lr fm_r)

  | otherwise
  = mkBranch 13{-which-} key elt fm_l fm_r

  where
    size_l = size fm_l
    size_r = size fm_r
\end{code}

--************************************************************************
--*									*
\subsubsection{Gluing two trees together}
--*									*
--************************************************************************

@glueBal@ assumes its two arguments aren't too far out of whack, just
like @mkBalBranch@.  But: all keys in first arg are $<$ all keys in
second.

\begin{code}
glueBal :: (Ord key)
	=> FiniteMap key elt -> FiniteMap key elt
	-> FiniteMap key elt

glueBal EmptyFM fm2 = fm2
glueBal fm1 EmptyFM = fm1
glueBal fm1 fm2
	-- The case analysis here (absent in Adams' program) is really to deal
	-- with the case where fm2 is a singleton. Then deleting the minimum means
	-- we pass an empty tree to mkBalBranch, which breaks its invariant.
  | size fm2 > size fm1
  = mkBalBranch mid_key2 mid_elt2 fm1 (deleteMin fm2)

  | otherwise
  = mkBalBranch mid_key1 mid_elt1 (deleteMax fm1) fm2
  where
    (mid_key1, mid_elt1) = findMax fm1
    (mid_key2, mid_elt2) = findMin fm2
\end{code}

@glueVBal@ copes with arguments which can be of any size.
But: all keys in first arg are $<$ all keys in second.

\begin{code}
glueVBal :: (Ord key)
	 => FiniteMap key elt -> FiniteMap key elt
	 -> FiniteMap key elt

glueVBal EmptyFM fm2 = fm2
glueVBal fm1 EmptyFM = fm1
glueVBal fm_l@(Branch key_l elt_l _ fm_ll fm_lr)
	 fm_r@(Branch key_r elt_r _ fm_rl fm_rr)
  | sIZE_RATIO * size_l < size_r
  = mkBalBranch key_r elt_r (glueVBal fm_l fm_rl) fm_rr

  | sIZE_RATIO * size_r < size_l
  = mkBalBranch key_l elt_l fm_ll (glueVBal fm_lr fm_r)

  | otherwise		-- We now need the same two cases as in glueBal above.
  = glueBal fm_l fm_r
  where
    (mid_key_l,mid_elt_l) = findMax fm_l
    (mid_key_r,mid_elt_r) = findMin fm_r
    size_l = size fm_l
    size_r = size fm_r
\end{code}

--************************************************************************
--*									*
\subsection{Local utilities}
--*									*
--************************************************************************

\begin{code}
splitLT, splitGT :: (Ord key) => FiniteMap key elt -> key -> FiniteMap key elt

-- splitLT fm split_key  =  fm restricted to keys <  split_key
-- splitGT fm split_key  =  fm restricted to keys >  split_key

splitLT EmptyFM split_key = empty
splitLT (Branch key elt _ fm_l fm_r) split_key
  | split_key < key = splitLT fm_l split_key
  | split_key > key = mkVBalBranch key elt fm_l (splitLT fm_r split_key)
  | otherwise	    = fm_l

splitGT EmptyFM split_key = empty
splitGT (Branch key elt _ fm_l fm_r) split_key
  | split_key > key = splitGT fm_r split_key
  | split_key < key = mkVBalBranch key elt (splitGT fm_l split_key) fm_r
  | otherwise	    = fm_r

findMin :: FiniteMap key elt -> (key,elt)
findMin (Branch key elt _ EmptyFM _) = (key,elt)
findMin (Branch key elt _ fm_l    _) = findMin fm_l

deleteMin :: (Ord key) => FiniteMap key elt -> FiniteMap key elt
deleteMin (Branch key elt _ EmptyFM fm_r) = fm_r
deleteMin (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt (deleteMin fm_l) fm_r

findMax :: FiniteMap key elt -> (key,elt)
findMax (Branch key elt _ _ EmptyFM) = (key,elt)
findMax (Branch key elt _ _    fm_r) = findMax fm_r

deleteMax :: (Ord key) => FiniteMap key elt -> FiniteMap key elt
deleteMax (Branch key elt _ fm_l EmptyFM) = fm_l
deleteMax (Branch key elt _ fm_l    fm_r) = mkBalBranch key elt fm_l (deleteMax fm_r)
\end{code}

--************************************************************************
--*									*
\subsection{Output-ery}
--*									*
--************************************************************************

\begin{code}

instance (Eq key, Eq elt) => Eq (FiniteMap key elt) where
  fm_1 == fm_2 = (size   fm_1 == size   fm_2) &&   -- quick test
		 (toList fm_1 == toList fm_2)

{- NO: not clear what The Right Thing to do is:
instance (Ord key, Ord elt) => Ord (FiniteMap key elt) where
  fm_1 <= fm_2 = (size   fm_1 <= size   fm_2) &&   -- quick test
		 (fmToList fm_1 <= fmToList fm_2)
-}

\end{code}


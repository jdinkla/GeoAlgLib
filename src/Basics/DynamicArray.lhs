%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Dynamische Arrays (|DynamicArray|)}
\module{DynamicArray}

> module Basics.DynamicArray (
>          Array, empty, fromList, insert, delete, 
>          lookup, lookupMany, getThe, getThem, nextIndex,
>          update, updateMany, Basics.DynamicArray.freeze, 
>          assocs, elems, indices, pprint, size, amap
>        ) where
> 
> import Prelude hiding (lookup)
> import Control.Monad.ST ( ST )
> import Data.Array.ST ( STArray )
> import Data.STRef ( STRef, newSTRef, readSTRef, writeSTRef )
> import Data.Array.MArray ( MArray, newArray, readArray, writeArray,
>             freeze, getBounds) 
> import qualified Data.Array (Array, array)
> import Data.Ix (Ix(range))
> import Data.Maybe (isJust, fromJust, catMaybes)
> import Basics.Pretty (Doc, vcat)

Wir benutzen ein Array, das bei Bedarf vergrößert wird \cite[K. 18.4]{cormen90:introduction}.
Unsere Funktion zum Löschen eines Elementes |delete| ist ``schwach'' \cite{klein97:cg},
da der Speicherplatz nicht wieder freigemacht wird. Andernfalls müßten
``Compaction''-Algorithmen implementiert werden.

Wir speichern den aktuellen Index und die Größe des Arrays.

>
> data Array s a 
>   = Array (STRef s (Int, Int, (STArray s Int (Maybe a))))
> 
> empty :: ST s (Array s a)
> empty = emptySized 1024
>
> emptySized :: Int -> ST s (Array s a) 
> emptySized size
>   = do a <- newArray (1, size) Nothing
>        x <- newSTRef (0, size, a)
>        return (Array x)
> 
> fromList :: [a] -> ST b (Array b a)
> fromList xs 
>   = do d@(Array a) <- emptySized size
>        (_, _, arr) <- readSTRef a
>        mapM_ (\ (i,v) -> writeArray arr i (Just v)) (zip [0..] xs)
>        writeSTRef a (n, size, arr)
>        return d
>   where
>   n = length xs
>   size = 2 ^ (floorlog2 n + 1)
>   floorlog2 x = if x>1 then 1 + floorlog2 (x `div` 2) else 0
> 
> nextIndex :: Array s a -> ST s Int
> nextIndex (Array a)
>   = do (i, _, _) <- readSTRef a
>        return (succ i)
> 
> size :: Array s a -> ST s Int
> size (Array a)
>   = do (i, _, _) <- readSTRef a
>        return i
> 
> insert :: Array s a -> a -> ST s Int
> insert d@(Array a) v
>   = do (i, s, ar) <- readSTRef a
>        m <- nextIndex d
>        if m > s then do enlarge d; insert d v
>                 else do writeArray ar m (Just v)
>                         writeSTRef a (m, s, ar)
>                         return m
> 
> enlarge :: Array s a -> ST s ()
> enlarge (Array da)
>   = do (i, s, a) <- readSTRef da
>        let s' = 2*s
>        a' <- newArray (1,s') Nothing
>        let is = range (1, s)
>        vs <- mapM (readArray a) is
>        mapM_ (\ (i,v) -> writeArray a' i (Just v)) (zip is (catMaybes vs))
>        writeSTRef da (i, s', a')
> 
> delete :: Array a b -> Int -> ST a ()       
> delete dynarr@(Array da) i
>   = do (i, s, a) <- readSTRef da
>        writeArray a i Nothing
> 
> lookup :: Array a b -> Int -> ST a (Maybe b)
> lookup (Array da) i
>   = do (_, _, a) <- readSTRef da
>        readArray a i
> 
> lookupMany :: Array s a -> [Int] -> ST s [Maybe a]
> lookupMany d is = mapM (lookup d) is
> 
> getThe :: Array s a -> Int -> ST s a
> getThe d i 
>   = do v <- lookup d i; return (fromJust v)
> 
> getThem :: Array s a -> [Int] -> ST s [a]
> getThem d is 
>   = mapM (getThe d) is
> 
> update :: Array a b -> (Int,b) -> ST a ()
> update d iv
>   = updateMany d [iv]
>        
> updateMany :: Array a b -> [(Int,b)] -> ST a ()
> updateMany (Array da) xs
>   = do (_, _, a) <- readSTRef da
>        mapM_ (\ (i,v) -> 
>          do v' <- readArray a i
>             if isJust v' then writeArray a i (Just v)
>                          else return ()) xs
>  
> freeze :: Array s a -> ST s (Data.Array.Array Int (Maybe a))
> freeze d@(Array da)
>   = do (i, s, a) <- readSTRef da
>        Data.Array.MArray.freeze a
>
> assocs :: Array s a -> ST s [(Int,a)]
> assocs (Array da)
>   = do (idx, size, arr) <- readSTRef da
> --       as <- accumulate [ mkPair arr i | i <- range (1,size) ]
>        as <- mapM (mkPair arr) (range (1,size))
>        return [ (i, fromJust v) | (i,v)<-as, isJust v ] 
>   where mkPair arr i = do v<-readArray arr i; return (i,v)
> 
> elems :: Array s a -> ST s [a]
> elems d
>   = do as <- assocs d; return (map snd as)
> 
> indices :: Array s a -> ST s [Int]
> indices d
>   = do as <- assocs d; return (map fst as)
> 
> imap :: (a -> a) -> Array s a -> ST s ()
> imap f d 
>   = do is <- indices d
>	 mapM_ (\ i -> do x <- getThe d i
>		          update d (i, f x)) is
>
> amap :: (a -> b) -> Array s a -> ST s (Array s b)
> amap f d@(Array da)
>   = do (idx, size, arr) <- readSTRef da
>        bounds <- getBounds arr
>        arr' <- newArray bounds Nothing
>        sequence [ do x <- readArray arr i
>                      writeArray arr' i (fmap f x) | i <- range bounds ]
>        da' <- newSTRef (idx, size, arr')	      
>        return (Array da')
>

%
% PRETTY-PRINTING
%
\subsubsection*{Pretty Printing}
 
> pprint :: ((Int,a) -> Doc) -> Array s a -> ST s Doc
> pprint pprintElem dynarr
>   = do as <- assocs dynarr
>        return (vcat (map pprintElem as))


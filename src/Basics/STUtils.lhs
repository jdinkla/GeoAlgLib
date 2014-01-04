
aus \cite{king96:functional}.

> module Basics.STUtils where
>
> import Control.Monad.ST ( ST )
> import Data.Array.ST ( STArray )
> import Data.Array.MArray ( MArray, newArray, readArray, writeArray,
>             freeze, getBounds) 
> import qualified Data.Array (Array, array)
> import Data.Ix (Ix (..))
>
> type Set s a = STArray s a Bool
>
> mkEmpty :: Ix a => (a,a) -> ST s (Set s a)
> mkEmpty bnds = newArray bnds False
>
> contains :: Ix a => Set s a -> a -> ST s Bool
> contains m v = readArray m v
>
> include :: Ix a => Set s a -> a -> ST s ()
> include m v  = writeArray m v True
>
> accumulate        :: Monad m => [m a] -> m [a] 
> accumulate []     = return [] 
> accumulate (c:cs) = do x <- c 
>                        xs <- accumulate cs 
>                        return (x:xs)

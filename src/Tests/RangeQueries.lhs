%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Bereichsabfragen (\texttt{RangeQueries})}
\module{tests/RangeQueries}

> module Main (main) where
> 
> import RBox      ( readPointsN )
> import Point     ( Point, PointN, pointN )
> import System.Environment    ( getArgs )
> import qualified KDTree as KD
> import qualified RangeTree as RT
> import Point     ( Point (dimension) )
> 
> data Which = KD | RT deriving Read
> 
> examine :: [String] -> IO (Which, String)
> examine [which, path] = return (read which, read path)
> examine _              = error "rangeQ: [KD|RT] filename\n"
> 
> main :: IO ()
> main = do args <- getArgs
>           (wh, file) <- examine args
>           (_,_,ps) <- readPointsN file
>           queryLoop wh ps
>             
> data Query a = Member [a] | Range ([a], [a]) | Quit deriving Read
>
> queryLoop :: (Num a, Read a, Ord a, Show a) => Which -> [PointN a] -> IO ()
> queryLoop KD ps
>   = query 
>   where
>   tree = KD.fromList ps
>   d    = dimension (head (KD.toList tree))
>   query = do putStr ">"; l <- getLine; handle (read l)
> 
>   handle (Member xs) 
>     = do putStrLn (if length xs /= d 
>                    then "Wrong dimension"
>                    else "=> " ++ show ((pointN xs) `KD.member` tree))
>          query
>   handle (Range (xs, ys))
>     = do putStrLn (if x/=d || y/=d then "Wrong dimension"
>                    else if x/=y then "dimensions in the query range differ"
>                    else "=> " ++ show (KD.rangeQuery tree (pointN xs, pointN ys)))
>          query
>     where x = length xs; y = length ys
>   handle Quit = return ()       

> queryLoop RT ps
>   = query 
>   where
>   tree = RT.fromList ps
>   d    = dimension (head (RT.toList tree))
>   query = do putStr ">"; l <- getLine; handle (read l)
> 
>   handle (Member xs) 
>     = do putStrLn (if length xs /= d 
>                    then "Wrong dimension"
>                    else "=> " ++ show ((pointN xs) `RT.member` tree))
>          query
>   handle (Range (xs, ys))
>     = do putStrLn (if x/=d || y/=d then "Wrong dimension"
>                    else if x/=y then "dimensions in the query range differ"
>                    else "=> " ++ show (RT.rangeQuery tree (pointN xs, pointN ys)))
>          query
>     where x = length xs; y = length ys
>   handle Quit = return ()       



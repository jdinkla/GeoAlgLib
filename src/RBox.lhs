%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Funktionen für das \texttt{rbox}-Format (|RBox|)}
\module{RBox}

> module RBox ( 
>          readPoints1, readPoints2, readPoints3, 
>          readPoints4, readPointsN, readWith, 
>	   writePoints
>        ) 
> where
> 
> import Point ( Point (..), Point1 (..), Point2 (..), Point3 (..), 
>                Point4 (..), PointN, pointN, toList )
>
> import IO    ( hPutStrLn, stdout, IOMode (WriteMode), openFile, hClose )
> 
> readP :: (Read a, Num a) => String -> IO (Int, Int, [[a]])
> readP name
>   = do xs <- readFile name
>        let (d:n:ys) = lines xs
>        return (read (head (words d)), read n, map (map read . words) ys)
> 
> readWith :: (Num a, Read a) => ([a] -> b) -> String -> IO (Int,Int,[b])
> readWith f name = do (d, n, xs) <- readP name; return (d, n, map f xs)
> 
> readPoints1 :: (Read a, Num a) => String -> IO (Int,Int,[Point1 a])
> readPoints1 = readWith (\ [x] -> Point1 x) 
> 
> readPoints2 :: (Read a, Num a) => String -> IO (Int,Int,[Point2 a])
> readPoints2 = readWith (\ [x,y] -> Point2 (x,y))
> 
> readPoints3 :: (Read a, Num a) => String -> IO (Int,Int,[Point3 a])
> readPoints3 = readWith (\ [x,y,z] -> Point3 (x,y,z))
> 
> readPoints4 :: (Read a, Num a) => String -> IO (Int,Int,[Point4 a])
> readPoints4 = readWith (\ [w,x,y,z] -> Point4 (w,x,y,z))
> 
> readPointsN :: (Read a, Num a) => String -> IO (Int,Int,[PointN a])
> readPointsN = readWith pointN

Wir brauchen nur eine Ausgabefunktion

> writePoints :: (Num a, Point p) => String -> [p a] -> IO ()
> writePoints name ps 
>   = do h <- openFile name WriteMode
>        let dim = dimension (head ps)
>	 hPutStrLn h (show dim)
>	 let xs = map toList ps
>	 hPutStrLn h (show (length xs))
>	 sequence (map (hPutStrLn h . cnc . (map show)) xs)
>	 hClose h
>   where cnc = foldr (\ s r -> s ++ ' ':r) "" 

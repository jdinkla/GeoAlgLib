%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%

> module Main ( main ) where
> 
> import DelaunayDAG
> import Applications.NearestPoint
> import RBox	    ( readPoints2 )
> import MetaPost
> import System.Environment ( getArgs )
> import System.Exit ( ExitCode (ExitFailure), exitWith )
> import Line	    ( Line ( Segment ) )
> import Point2	    ( Point (mapP), Point2 (..), P2 )
> import Basics.DoubleEps
>
> data Mode = N | DE deriving (Eq, Read, Show)
>
> examine :: [String] -> IO (Mode, Double, String, String)
> examine [mode, scale, file1, file2]
>   = return (read mode, read scale, file1, file2)
> examine _ 
>   = do putStrLn "Synopsis: nearest {N|DE} scale file1 file2\n"
>        exitWith (ExitFailure 1)
>
> main :: IO ()
> main = do args <- getArgs
>           (m, sc, f1, f2) <- examine args
>           (_, _, ps) <- readPoints2 f1
>           (_, _, qs) <- readPoints2 f2
>	    putStrLn ("beginfig(1);\npicture p;" ++ pen 3)
>	    putMP [Scaled sc] ps 
>	    putMP [Scaled sc, red] qs
>	    putStrLn (pen 0.5)
>	    if m==N then do let dag = delaunay ps
>--			    putMP [Scaled sc] (map MP_Triangle (triangles dag))
>			    putMP [Scaled sc] (triangles dag)
>			    putStrLn (pen 1)
>			    putMP [Scaled sc, green] 
>--				  [MP_Line (Segment p (nearestPoint dag p)) | p <- qs ]
>				  [Segment p (nearestPoint dag p) | p <- qs ]
>		    else do let dag = delaunay (dbl ps)
>--			    putMP [Scaled sc] (map MP_Triangle (triangles dag))
>			    putMP [Scaled sc] (triangles dag)
>			    putStrLn (pen 1)
>			    putMP [Scaled sc, green] 
>--				  [MP_Line (Segment p (nearestPoint dag p)) | p <- (dbl qs) ]
>				  [Segment p (nearestPoint dag p) | p <- (dbl qs) ]
>	    putStrLn ("endfig;\nend")
>           		    
>
> dbl :: [P2 Double] -> [P2 DoubleEps]
> dbl = map (mapP DoubleEps) 
>

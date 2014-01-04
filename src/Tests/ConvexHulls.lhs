%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Konvexe Hüllen (\texttt{ConvHulls})}
\module{tests/ConvHulls}

> module Main ( main ) where
> 
> import GrahamsScan ( graham, graham2, graham3 )
> import JarvisMarch ( jarvis, jarvisInt )
> import MergeHull   ( mergeHull, bottomupMergeHull )
> import KirkpatrickSeidelHull ( ksHull, ksHullX )
> import ChansConvexHull ( chan )
> import MetaPost
> import RBox        ( readPoints2 )
> import System      ( ExitCode (ExitFailure), exitWith, getArgs )
> import Maybe	     ( fromJust )
> --import Compat      ( getArgs )
> import Point (Point (mapP))
> import Point2 (P2)
> import Polygon (mapPolygon)
> import Basics.OrderStat   ( median )
> import Basics.Utilities   ( longerThan )
>
> data Alg = G|G3|J|JI|M|BM|K|K2|C deriving (Eq, Enum, Read)
>
> algo n = fromJust (lookup n (zip [2..] [G ..]))
>
> examine :: [String] -> IO (Double, [Alg], String)
> examine [scale, nots, filename] 
>   = return (read scale, read nots, filename)
> examine _ 
>   = do putStrLn "convexhulls: scale [G,G3,J,JI,M,BM,K,K2,C] filename"
>        exitWith (ExitFailure 1)
> 
> main :: IO ()
> main = do args <- getArgs
>           (s, l, f) <- examine args
>           (_, _, ps) <- readPoints2 f
>	    putStrLn (figure 1 (points s ps))
>           out 2 s l  (_scc_ "graham"    (graham ps))
>           out 3 s l  (_scc_ "graham3"	(graham3 ps))
>           out 4 s l  (_scc_ "jarvis"	(jarvis ps))
>           out 5 s l  (_scc_ "jarvisint" (jarvisInt ps))
>           out 6 s l  (_scc_ "mergehull" (mergeHull ps))
>           out 7 s l  (_scc_ "bumerge"	(bottomupMergeHull ps))
>           out 8 s l  (_scc_ "kshull"	(ksHull ps))
>           out 9 s l  (_scc_ "kshull2"	(ksHullX (median.filt) (median.filt) ps))
>           out 10 s l (_scc_ "chan"	(chan ps))
>	    putStrLn "\n end"
> 
> out n s l p
>   | algo n `elem` l = putStrLn (figure n (convexhull s p))
>   | otherwise	      = putStrLn (figure n "draw (0,0) withcolor white;\n")
>           
> points sc ps
>   = "%points\n" ++ "picture p;\n" ++ pen 1 ++ showMP [Scaled sc] ps ++ "p := currentpicture;\n"
> 
> convexhull sc poly
> -- ifndef MPC
> --  = "%convex hull\n" ++ "draw p;\n" ++ pen 0.5 ++ showMP [Scaled sc, red] (MP_Polygon poly) ++ "\n"
> --else
>   = "%convex hull\n" ++ "draw p;\n" ++ pen 0.5 ++ showMP [Scaled sc, red] poly ++ "\n"
> --endif
>
> filt :: [a] -> [a]
> filt xs
>   | xs `longerThan` 10 = tk 1 xs 
>   | otherwise          = xs
>   where
>   tk i []     = []
>   tk i [x]    = []
>   tk i (x:y:xs) = x : y : tk (2*i) (skip i xs)
>   skip 0 xs	= xs
>   skip k []	= []
>   skip k (x:xs) = skip (k-1) xs







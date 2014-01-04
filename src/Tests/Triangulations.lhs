%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Triangulationen (|Triangulations|)}
\module{tests/Triangulations}

> module Main (main) where
>
> import ConvexTriangulation   ( convexTri ) 
> import NaiveTriangulation    ( earExcision, naiveTri )
> import MonotoneTriangulation ( monoTri )
> import GareysTriangulation   ( garey )
> import KETTriangulation      ( ketTri )
> import AdaptTriangulation    ( adaptTri )
> import MergeTriangulation    ( mergeTri )
> import Point2		       ( Point2 (..), rotateOrg )
> import Polygon	       ( Polygon, vertices, mapPolygon )
> import RPG		       ( readPolygon )
> import System.Environment ( getArgs )
> import System.Exit           ( ExitCode (ExitFailure), exitWith )
> import Data.Maybe            ( fromJust )
> import MetaPost
> import Triangle	       ( Triangle2 )
> import Polygon	       ( Polygon2 )
>
> --import Polygon (Polygon (..))
> --import Point2 (Point (..))
> --import Polys 
>
> data Mode = Var | Mono  deriving (Eq, Read)
>
> examine :: [String] -> IO (Mode, [Alg], Double, String)
> examine [mode, algs, scale, filename] 
>   = return (read mode, read algs, read scale, filename)
> examine _ 
>   = do putStrLn "Synopsis: benchTri {Var|Mono} [E|N|M|G|K|A|C|MT] scale file\n"
>        exitWith (ExitFailure 1)
>        
> data Alg = E|N|M|G|K|A|C|MT deriving (Eq,Enum,Read)
>
> algo :: Int -> Alg
> algo n = fromJust (lookup n (zip [2..] [E ..]))
>
> main :: IO ()
> main = do args <- getArgs
>           (md, l, sc, file) <- examine args
>           p' <- readPolygon file
>	    p <- return (if md==Mono then mapPolygon trans p' else p')
>           let ps = vertices p
>	    putStrLn (figure 1 (polygon2 sc p (points sc ps "picture p; p:=currentpicture;\n")))
>	    out  2 sc l (_scc_ "earEx" (earExcision p))
>	    out  3 sc l (_scc_ "naiveTri" (naiveTri p))
>	    out  4 sc l (_scc_ "monoTri" (monoTri p))
>	    out  5 sc l (_scc_ "garey" (garey p))
>	    out  6 sc l (_scc_ "ketTri" (ketTri p))
> 	    out  7 sc l (_scc_ "adaptTri" (adaptTri p))
> 	    out  8 sc l (_scc_ "convexTri" (convexTri p))
> 	    out  9 sc l (_scc_ "mergeTri" (mergeTri (vertices p)))
>	    putStrLn "\n end"
>
> out n sc l ts
>   | (algo n) `elem` l = putStrLn (figure n ("draw p;\n" ++ triangles sc ts (triangles2 sc ts "")))
>   | otherwise	        = putStrLn (figure n "draw (0,0) withcolor white;\n")
>
> trans :: (Ord a, Floating a) => Point2 a -> Point2 a
> trans p = rotateOrg p (-pi/2)
>
> points sc ps cs
>   = "%points\n" ++ pen 2 ++ showMP [Scaled sc] ps ++ "\n" ++ cs
> 
> polygon, polygon2 :: (RealFloat a, Show a) => Double -> Polygon2 a -> String -> String  
> triangles, triangles2 :: (RealFloat a, Show a) => Double -> [Triangle2 a] -> String -> String
>
> 
> polygon sc p cs
>   = "%polygon\n" ++ pen 0.5 ++ showMP [Scaled sc] p ++ "\n" ++ cs
>
> polygon2 sc p cs
>   = "%polygon\n" ++ pen 5 ++ showMP [Scaled sc, Color (0.7,0.7,0.7)] p ++ "\n" ++ cs
>
> triangles sc ts cs
>   = "%triangles\n" ++ pen 0.5 ++ showMP [Scaled sc, Filled, Color (0.9,0.9,0.9)] ts ++ "\n" ++ cs
>
> triangles2 sc ts cs
>   = "%triangles\n" ++ pen 0.5 ++ showMP [Scaled sc, Dashed] ts ++ "\n" ++ cs
>
 
 numVerts poly = mapM_ (\ (i,p) -> putStrLn ("label(\"" ++ show i ++ "\"," 
				            ++ show p ++ " scaled 10cm);")) 
                       (zip [0..] (vertices poly))


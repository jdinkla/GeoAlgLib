%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998.
%
\subsection{Delaunay-- und Voronoidiagramme (|VoronoiDelaunay|)}
\module{tests/VoronoiDelaunay}

> module Main ( main ) where
>
> import QEDSbasics
> import qualified  QEDSstatic as S
> import Delaunay   ( delaunay, delaunayST, DelEdge )
> import qualified Delaunay as D
> import Voronoi    ( voronoi, voronoiEdges, Edges (..) )
> import qualified DelaunayDAG as DDAG
> import RBox	    ( readPoints2 )
> import MetaPost
> import System.Environment ( getArgs )
> import System.Exit    ( ExitCode (ExitFailure), exitWith )
> import Data.Maybe	    ( fromJust )
> import Line	    ( Line ( Segment ) )
> import Point2	    ( Point (mapP), Point2 (..), P2, xcoord, ycoord )
> import Basics.DoubleEps
> import Prelude    hiding (flip)
> import Control.Monad	    ( when )
> import Triangle   ( Triangle2 )
> import Numeric    ( showFFloat )
>
> data Mode = N | DE deriving (Eq, Read, Show)
>
> data Alg = P|DC|Vor|Inc|DAG deriving (Eq,Enum,Read)
>
> algo n = fromJust (lookup n (zip [2..] [DC ..]))
>
> examine :: [String] -> IO (Mode, [Alg], Double, String)
> examine [mode, algs, scale, filename]
>   = return (read mode, read algs, read scale, filename)
> examine _
>   = do putStrLn "Synopsis: prg {N|DE} [P|DC|Vor|Inc|DAG] scale file\n"
>        exitWith (ExitFailure 1)
>
> main :: IO ()
> main = do args <- getArgs
>           (m, as, sc, f) <- examine args
>           (_, _, ps) <- readPoints2 f
>	    let qs = dbl ps
>	    when (P `elem` as) (outPoints sc ps)
>	    when (DC `elem` as) (
>	      if m==N then outDC sc     ({-# SCC "delDC" #-} (delaunay ps))
>		          else outDC sc     ({-# SCC "delDC_DE" #-} (delaunay qs)))
>	    when (Vor `elem` as) (
>	      if m==N then outVor sc ps ({-# SCC "vorDC" #-} (voronoi ps))
>		          else outVor sc qs ({-# SCC "vorDC_DE" #-} (voronoi qs)))
>           when (DAG `elem` as) (
>	      if m==N then outDAG sc    ({-# SCC "delDAG" #-} (DDAG.triangles (DDAG.delaunay ps)))
>		          else outDAG sc    ({-# SCC "delDAG_DE" #-} (DDAG.triangles (DDAG.delaunay qs))))
>	    putStrLn "\n end"
>
> dbl :: [P2 Double] -> [P2 DoubleEps]
> dbl = map (mapP DoubleEps)
>
> outPoints :: (RealFloat a, Show a) => Double -> [P2 a] -> IO ()
> outPoints sc ps
>   = do putStrLn ("beginfig(1);\npicture p;" ++ pen 3)
>	 putMP [Scaled sc] ps
>	 putStrLn ("p := currentpicture;\nendfig;\n")
>
> outDC :: (RealFloat a, Show a) => Double -> (S.QEDS (DelEdge (P2 a)), b, b) -> IO ()
> outDC sc (q,_,_)
>   = do putStrLn ("beginfig(2);\ndraw p;" ++ pen 0.5)
>	 putMP [Scaled sc, green] [Segment x y | (x,y) <- edges es]
>	 putStrLn ("endfig;\n")
>   where es = map (attributes.snd) (S.connected q)
>	  edges = map (\ e -> (D.source e, D.target e))
>
> outVor :: (RealFloat a, Show a) => Double -> [P2 a] -> (S.QEDS (Edges (P2 a)), b, b) -> IO ()
> outVor sc ps (q,_,_)
>   = do putStrLn ("beginfig(3);\ndraw p;" ++ pen 0.5)
>	 putMP [Scaled sc, green] [Segment x y | (x,y) <- edges es]
>	 putMP [Scaled sc, red] [x | x <- voronoiEdges es]
>	 putStrLn ("clip currentpicture to (bbox (" ++ show (xmin, ymin)
>		    ++ "--" ++ show (xmax, ymax) ++ ") scaled " ++ showFFloat (Just 3) sc "cm);")
>	 putStrLn ("endfig;\n")
>   where es = map (attributes.snd) (S.connected q)
>	  edges = map (\ e -> (source e, target e))
>	  xs = map xcoord ps
>	  ys = map ycoord ps
>	  xmin = 1.5 * minimum xs
>	  xmax = 1.5 * maximum xs
>	  ymin = 1.5 * minimum ys
>	  ymax = 1.5 * maximum ys


> outDAG :: (RealFloat a, Show a) => Double -> [Triangle2 a] -> IO ()
> outDAG sc ts
>   = do putStrLn ("beginfig(4);\ndraw p;" ++ pen 0.5)
>	 putMP [Scaled sc, blue] ts
>	 putStrLn ("endfig;\n")


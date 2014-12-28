--------------------------------------------------------------------------------
-- Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
--------------------------------------------------------------------------------
--
-- see
--     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
--     University of Bonn, Germany, 1998.
--
module RPG (
      readPolygon, readPoints
       ) where

import Point   ( Point2 (..) )
import Polygon ( Polygon (..), Polygon2 )
import Data.Array   ( listArray, (!) )

readPolygon :: (Read a, Num a, Eq a) => String -> IO (Polygon2 a)
readPolygon name = do
    xs <- readFile name
    let (_:ns:ys) = filter (not . isComment) (lines xs)
        n = (read ns) :: Int
        (ps, (_:"POLYGON":is)) = splitAt n ys
        arr = listArray (1,n) (points ps)
    return (PolygonCCW (map ((arr!) . read) is))

readPoints :: (Read a, Num a, Eq a) => String -> IO [Point2 a]
readPoints name = do
    xs <- readFile name
    let (_:ns:ys) = filter (not . isComment) (lines xs)
        n = read ns
        (ps, (_:"POLYGON":_)) = splitAt n ys
    return (points ps)

isComment :: String -> Bool
isComment "" = True
isComment ('#':_) = True
isComment _ = False

points            :: (Read a, Num a, Eq a) => [String] -> [Point2 a]
points xs                = map ((\ [x,y] -> Point2 (read x, read y)) . words) xs


--Ausgabefunktionen fehlen noch







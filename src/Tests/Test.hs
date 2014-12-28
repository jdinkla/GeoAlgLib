--------------------------------------------------------------------------------
-- Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
--------------------------------------------------------------------------------
--
-- see
--     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
--     University of Bonn, Germany, 1998.
--
module Tests.Test ( module Tests.Test, module Basics.Random ) where

import Point   ( Point1 (..), Point2 (..), Point3 (..), Point4 (..), pointN,
		 PointN (..), Point ((<*>)) )
import Data.List    ( zip4 )
import Basics.Random  ( randomInts, randomDoubles )
import Basics.Utilities ( splitsAt )

--
-- PSEUDO-ZUFLLIGE MENGEN VON PUNKTEN
--

modulo n = map (\ x -> fromIntegral (x `mod` n))
scale k = map (\ x -> k*x)
split d  = splitsAt d

to1 :: Num a => [[a]] ->  [Point1 a]
to1 = map (\ [x] -> Point1 x)

to2 :: (Num a, Eq a) => [[a]] ->  [Point2 a]
to2 = map (\ [x,y] -> Point2 (x,y))

to3 :: (Num a, Eq a) => [[a]] ->  [Point3 a]
to3 = map (\ [x,y,z] -> Point3 (x,y,z))

to4 :: Num a => [[a]] ->  [Point4 a]
to4 = map (\ [w,x,y,z] -> Point4 (w,x,y,z))

toN :: Num a => [[a]] ->  [PointN a]
toN = map pointN

ips1 k n = take n . to1 . split 1 . modulo k
ips2 k n = take n . to2 . split 2 . modulo k
ips3 k n = take n . to3 . split 3 . modulo k
ips4 k n = take n . to4 . split 4 . modulo k
ipsN d k n = take n . toN . split d . modulo k

dps1 k n = take n . to1 . split 1 . scale k
dps2 k n = take n . to2 . split 2 . scale k
dps3 k n = take n . to3 . split 3 . scale k
dps4 k n = take n . to4 . split 4 . scale k
dpsN d k n = take n . toN . split d . scale k


addD ps
  = Point2 (0,ym) : Point2 (xm,0) : Point2 (0, -ym) : Point2 (-xm, 0) : ps
  where
  xm = 2000
  ym = 2000

pointsOnC c r n               = take n (map ((c+) . (r<*>)) pointsOnUnitCircle)

pointsOnUnitCircle            = map point rands
  where rands                 = map (*(2*pi)) (randomDoubles 511 137)
        point phi             = Point2 (cos phi, sin phi)

pointsInC c r n               = take n (map ((c+) . (r<*>)) pointsInUnitCircle)

pointsInUnitCircle            = (zipWith point rands rands2)
  where rands                 = map (*(2*pi)) (randomDoubles 511 137)
        rands2                = randomDoubles 247 1031
        point phi d           = Point2 (d * cos phi, d * sin phi)

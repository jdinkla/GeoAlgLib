%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%

> module Pertub (noXdups, noYdups) where
> 
> import Point2  ( Point2D, Point2 (..), rotateOrg, leftOrBelow, belowOrLeft, 
>		   equalX, equalY, angle2 )
> import Data.List	 ( groupBy )
> import Basics.Utilities ( isSingleton )
> import Basics.Sorting ( sortBy )

> type Pertub a b c d           = a -> (c -> d, b)
 
|noXdups| und |noYdups| rotieren eine Punktmenge so, daß keine zwei Punkte die gleichen $x$
bzw. $y$ Koordinaten haben (nach \cite[S. 74--75]{klein97:cg}).

> noXdups, noYdups              :: Pertub [Point2D] [Point2D] [Point2D] [Point2D]
> noXdups ps                    = case xAngle ps of
>                                   Nothing -> (id, ps)
>                                   Just angle -> ( map (flip rotateOrg (-angle)), 
> 						  map (flip rotateOrg angle) ps )
>   where rot phi		      = map (flip rotateOrg phi) 
> 
> noYdups ps                    = case yAngle ps of
>                                   Nothing -> (id, ps)
>                                   Just angle -> (rot (-angle), rot angle ps)
>   where rot phi               = map (flip rotateOrg phi) 
> 				    
> xAngle, yAngle                :: [Point2D] -> Maybe Double
> xAngle                        = rotationAngle leftOrBelow equalX (Point2 (0,-1))
> yAngle                        = rotationAngle belowOrLeft equalY (Point2 (-1,0))
> 
> rotationAngle                 :: (Point2D -> Point2D -> Bool) 
>                                  -> (Point2D -> Point2D -> Bool) 
>                                  -> Point2D -> [Point2D] -> Maybe Double
> rotationAngle less rel v ps
>   | isSingleton grouped       = Just (- (pi/2))
>   | all isSingleton grouped   = Nothing
>   | otherwise                 = Just (- (minimum angles / 2))
>   where sorted                = sortBy less ps
>         grouped               = groupBy rel sorted
>         angles                = map angle' (connect grouped)
>         angle' (p,q)          = angle2 v (q-p)
>         connect (x:y:ys)      = (last x, head y) : connect (y:ys)
>         connect _             = []


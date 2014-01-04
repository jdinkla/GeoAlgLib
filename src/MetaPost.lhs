%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Funktionen zur Ausgabe in MetaPost (|MetaPost|)}
\module{MetaPost}

>
> module MetaPost (
>          MetaPost (..), Option (..), 
>	   defaults, putMP, showMP, clip, pen, figure,
>	   red, green, blue, yellow, purple, cyan, white, black
>        )
> where 
> 
> import Point2    ( Point2 (..), Point (mapP), distance )
> import Line	   ( Line (..), Line2 )
> import Triangle  ( Triangle (..) )
> import Polygon   ( Polygon (..), vertices )
> import Circle    ( Circle (..) )
> import Numeric   ( showFFloat )
> import List	   ( intersperse )
> import Basics.Sorting   ( sort )
> import Basics.Utilities ( splitsAt )
> import Basics.ListCat
> 
> data Option                   = None
> 			        | Scaled Double
> 			        | Dashed
> 			        | Color (Double, Double, Double)
> 			        | Filled
> 			        deriving (Eq, Ord)
>
> class MetaPost m where
>     mp                        :: [Option] -> m -> ListCat Char 
>
> instance (Show a, RealFloat a) => MetaPost (Point2 a) where
>     mp os p                   = putl os (showPoint p) 
>
> instance MetaPost a => MetaPost [a] where
>     mp os xs                  = cats (map (\ x -> mp os x `cat` (unit '\n')) xs)

{-
ifndef MPC

 newtype MP_Line a		= MP_Line (Line Point2 a)
 newtype MP_Triangle a		= MP_Triangle (Triangle Point2 a)
 newtype MP_Polygon a		= MP_Polygon (Polygon Point2 a)
 newtype MP_Circle a           = MP_Circle (Circle Point2 a)

 instance (Show a, RealFloat a) => MetaPost (MP_Line a) where
     mp os (MP_Line s)	        = putl os (case s of
				    Segment s t -> showLine s t
				    Ray s t	-> let d = distance s t / 2000
						   in showLine s (s + (mapP (/d) (t-s)))
				    Line s t	-> showLine s t )
 
 instance (Show a, RealFloat a) => MetaPost (MP_Triangle a) where
     mp os (MP_Triangle (Triangle (p,q,r)))  
				= putl os (cats [showPoint p, toListCat "--", showPoint q, 
 					       toListCat "--", showPoint r, toListCat "--cycle"])
 					       
 instance (Show a, RealFloat a) => MetaPost (MP_Polygon a) where
     mp os (MP_Polygon p)      = putl os (cnct' (map cnct vss) `cat` toListCat "--cycle")
       where vss	        = splitsAt 100 (vertices p)
 	      cnct   	        = cats . intersperse (toListCat "--") . map showPoint
 	      cnct'   	        = cats . intersperse (toListCat "\n --")

 instance (Show a, RealFloat a) => MetaPost (MP_Circle a) where
     mp os (MP_Circle (Circle c r)) = putl os (cats [toListCat "fullcircle scaled ", 
						      showFixed (2 * sqrt r), toListCat " shifted ",
						      showPoint c])

else
-}
 
> instance (Show a, RealFloat a) => MetaPost (Line Point2 a) where
>     mp os (Segment s t)       = putl os (showLine s t)
>     mp os (Ray s t)           = putl os (showLine s (s + (mapP (/d) (t-s))))
>       where d                 = distance s t / 2000
>     mp os (Line s t)          = putl os (showLine s t)
> 
> instance (Show a, RealFloat a) => MetaPost (Triangle Point2 a) where
>     mp os (Triangle (p,q,r))  = putl os (cats [showPoint p, toListCat "--", showPoint q, 
> 					       toListCat "--", showPoint r, toListCat "--cycle"])
>
> instance (Show a, RealFloat a) => MetaPost (Polygon Point2 a) where
>     mp os p		        = putl os (cnct' (map cnct vss) `cat` toListCat "--cycle")
>       where vss	        = splitsAt 100 (vertices p)
> 	      cnct   	        = cats . intersperse (toListCat "--") . map showPoint
> 	      cnct'   	        = cats . intersperse (toListCat "\n --")
>
> instance (Show a, RealFloat a) => MetaPost (Circle Point2 a) where
>     mp os (Circle c r)        = putl os (cats [toListCat "fullcircle scaled ", 
>					         showFixed (2 * sqrt r), toListCat " shifted ",
>						 showPoint c])

{-endif-}

> defaults                      :: [Option]
> defaults                      = [Scaled 1.0]
> 
> options                       :: [Option] -> ListCat Char
> options xs                    = cats (map opt (sort xs))
>   where opt None	        = nil 
> 	  opt (Scaled sc)       = cats [toListCat " scaled ", showFixedD 3 sc, toListCat "cm"]
> 	  opt (Dashed)		= toListCat " dashed evenly"
> 	  opt (Color (r,g,b))	= cats [toListCat " withcolor (", showFixedD 2 r, unit ',', 
>				        showFixedD 2 g, unit ',', showFixedD 2 b, unit ')']
>         opt Filled		= nil
>
> showFixed		        :: RealFloat a => a -> ListCat Char
> showFixed x                   = showFFloat (Just 3) x
> 
> showFixedD		        :: RealFloat a => Int -> a -> ListCat Char
> showFixedD d x                = showFFloat (Just d) x
> 
> showPoint                     :: RealFloat a => Point2 a -> ListCat Char
> showPoint (Point2 (x,y))      = cats [unit '(', showFixed x, unit ',', showFixed y, unit ')']
> 
> showLine		        :: RealFloat a => Point2 a -> Point2 a -> ListCat Char
> showLine x y		        = showPoint x `cat` (toListCat "--" `cat` showPoint y)
>
> putl			        :: [Option] -> ListCat Char -> ListCat Char
> putl os xs                    = cats [ toListCat (if Filled `elem` os then "fill (" else "draw ("),
> 				       xs, unit ')', options os, unit ';']
> 
> putMP			        :: MetaPost a => [Option] -> a -> IO ()
> putMP os xs		        = putStrLn (list (mp os xs))
> 
> showMP	 		:: MetaPost a => [Option] -> a -> String
> showMP os xs		        = list (mp os xs)
> 
> figure                        :: Int -> String -> String
> figure i str                  = "beginfig(" ++ show i ++ ");\n" ++ str ++ "endfig;"
> 
> pen                           :: Double -> String
> pen n                         = "pickup pencircle scaled " ++ show n ++ "pt;\n"
> 
> clip			        :: Double -> (Double, Double, Double, Double) -> String
> clip sc (x,y,u,v)	        = "path p;\np := (" ++ p 
> 			          ++ ")" ++ list (options [Scaled sc])
> 			          ++ ";\nclip currentpicture to (bbox p);\n"
>   where p		        = concat (intersperse "--" ls)
>         ls		        = [show (x,y), show (u,y), show (u,v), show (x,v), "cycle"]
>
> red, green, blue, yellow, 
>   purple, cyan, white, black	:: Option
> red				= Color (1,0,0)
> green				= Color (0,1,0)
> blue				= Color (0,0,1)
> yellow			= Color (1,1,0)
> purple			= Color (1,0,1)
> cyan				= Color (0,1,1)
> black				= Color (0,0,0)
> white				= Color (1,1,1)

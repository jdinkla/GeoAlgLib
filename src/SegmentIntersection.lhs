%------------------------------------------------------------------------------
% Copyright (C) 1997, 1998, 2008 Joern Dinkla, www.dinkla.net
%------------------------------------------------------------------------------
%
% see
%     Joern Dinkla, Geometrische Algorithmen in Haskell, Diploma Thesis,
%     University of Bonn, Germany, 1998. 
%
\subsection{Anwendung: Schnittpunkte von Strecken (\texttt{SegmentIntersection})}
\module{SegmentIntersection}

siehe \cite[K. 2.3.1, K.2.4]{klein97:cg},
\cite[K. 2.1]{berg97:cg} \textbf{wird in der Arbeit nicht behandelt}.

\begin{code}
module SegmentIntersection (
         naiveIntersections, intersections
       ) where

import Point2
import Line (Line (Line, Segment), intersect, strictIntersect, 
              point1, point2, vertical, Line2)
import qualified EventStructureSpec as ES
import qualified StatusStructureSpec as SS
import Maybe (catMaybes)
import Basics.Sorting (nubSorted, merge)

import MetaPost

\end{code}

Der naive $O(n^2)$ Algorithmus \Index{naiveIntersections} testen die $O(n^2)$ Paare von
Strecken auf einen Schnittpunkt.

\begin{code}
naiveIntersections ls         = catMaybes (map (uncurry intersect) (upper_pairs ls))
  where upper_pairs []        = []
        upper_pairs (x:xs)    = [(x,y) | y<-xs] ++ upper_pairs xs
\end{code}

Wir folgen dem Plane-Sweep Algorithmus aus \cite[K.2.32]{klein97:cg} und definieren folgende
Ereignistypen:

\begin{code}
data Type a                   = LeftEnd (Line2 a)
                              | RightEnd (Line2 a)
                              | Intersection (Line2 a) (Line2 a) (Point2 a)
\end{code}

Wir ``sweepen'' entlang der $x$-Achse und, da Ereignisse vom Typ \texttt{Intersection} im Verlauf
des Algorithmus erzeugt werden, benötigen wir eine dynamische Ereignisstruktur.

Ereignisse werden lexikographisch geordnet. Um die Ereignisstruktur zu benutzen, benötigen
wir eine Instanz von \texttt{Ord}.
\begin{code}
instance (Num a, Ord a) => Ord (Point2 a) where
    compare (Point2 (x,y)) (Point2 (x',y'))
      | x<x' = LT
      | x==x' && y<y' = LT
      | x==x' && y==y' = EQ
      | otherwise = GT
\end{code}

\begin{code}
type Es a = ES.ES (Point2 a) (Type a)
type Ss a = SS.SS (Point2 a) (Line2 a)
type Ps a = [Point2 a]
\end{code}

1. Die Event--Struktur darf keine gleichen Ereignisse enthalten !
2. optimaler Speicherplatzverbrauch, wenn nur Schnittpunktereignisse von 
   in der SSS benachbarten Elementen gespeichert werden.


\begin{code}
intersections                 :: (Ord a, Fractional a) => [Line2 a] -> [Point2 a]
intersections segs            = sweep es ss
  where es                    = ES.fromList (events segs)
        events                = concatMap evs
          where evs sg@(Segment s t) 
                              = [ ES.Event m (LeftEnd (Segment m n)),
                                  ES.Event n (RightEnd (Segment m n)) ]
                  where (m,n) = if s < t then (s,t) else (t,s)
        ss                    = SS.empty
\end{code}

\begin{code}
sweep                         :: (Ord a, Fractional a) => Es a -> Ss a -> [P2 a]
sweep es ss
  | ES.isEmpty es             = []
  | otherwise                 = handle (ES.event e)

  where e                     = ES.head es
        es1                   = ES.tail es
        p                     = ES.time e

        handle (LeftEnd s)    = sweep es3 ss1
          where ss1           = insertSegment ss s
                es2           = test es1 p (SS.pred ss1 p) (Just (undefined,s))
                es3           = test es2 p (Just (undefined,s)) (SS.succ ss1 p)

        handle (RightEnd s)   = sweep es2 ss1
          where q             = point1 s
                ss1           = SS.delete ss q
                es2           = test es1 p (SS.pred ss q) (SS.succ ss q)

        handle (Intersection s t u) = u : sweep es3 ss1
          where q             = point1 s
                r             = point1 t
                ss1           = SS.swap ss q r
                es2           = test es1 p (SS.pred ss1 r) (Just (undefined, t))
                es3           = test es2 p (Just (undefined, s)) (SS.succ ss1 q)

\end{code}

\begin{code}
insertSegment                 :: (Ord a, Fractional a) => Ss a -> Line2 a -> Ss a
insertSegment ss (Segment s t)
                              = SS.insertBy less ss (s, Segment s t)
  where less _ (_, sg)	      = case intersect (vertical (xcoord s)) sg of
                                  Nothing -> LT
                                  Just r -> compare s r
\end{code}

\Forget{
\begin{code}
test                          :: (Ord a, Fractional a) => Es a -> (Point2 a) 
                                 -> Maybe (P2 a, Line2 a) -> Maybe (P2 a,Line2 a) -> Es a
test es p s t                 = maybe es (ES.insert es) intersection
  where intersection          = do (_,seg1) <- s; (_,seg2) <- t
                                   q <- strictIntersect seg1 seg2
                                   if q > p
                                     then return (ES.Event q 
                                                 (Intersection seg1 seg2 q))
                                     else Nothing
\end{code}

\begin{code}
tlines :: Num a => [Line2 a]
tlines = [ Segment (Point2 (2,2)) (Point2 (10,3)),
           Segment (Point2 (3,8)) (Point2 (12,5)),
           Segment (Point2 (7,7)) (Point2 (14,9)),
           Segment (Point2 (1,12)) (Point2 (8,5)),
           Segment (Point2 (4,5)) (Point2 (12,9)) ]

tlines2 :: Num a => [Line2 a]
tlines2 = [ Segment (Point2 (1,1)) (Point2 (7,1)),
            Segment (Point2 (1,3)) (Point2 (7,3)),
            Segment (Point2 (1,5)) (Point2 (7,5)),
            Segment (Point2 (1,7)) (Point2 (7,7)),
            Segment (Point2 (2,6)) (Point2 (6,2)),
            Segment (Point2 (3,0)) (Point2 (3,8)),
            Segment (Point2 (3,5)) (Point2 (6,7)),
            Segment (Point2 (6,0)) (Point2 (6,8)),
            Segment (Point2 (2,2)) (Point2 (6,6)) ]

tlines3 :: Num a => [Line2 a]
tlines3 = [
            Segment (Point2 (1,1)) (Point2 (1,7)),
            Segment (Point2 (5,1)) (Point2 (5,7)),
            Segment (Point2 (0,3)) (Point2 (7,3)),
            Segment (Point2 (0,5)) (Point2 (7,5))
          ]
\end{code}


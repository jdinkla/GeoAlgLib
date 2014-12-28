module Basics.FiniteMap (
    FiniteMap,
    insertManyC,
    module M
) where

import Data.Map.Strict as M

type FiniteMap = Map

insertManyC f m l = unionWith f m $ fromList l

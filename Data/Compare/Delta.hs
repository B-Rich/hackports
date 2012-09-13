module DifferenceSet where

import Element

data DeltaMode = Added | Changed | Removed deriving (Show, Ord, Eq)

data Delta a = Delta {
      deltaMode       :: DeltaMode
    , previousElement :: Element a
    , currentElement  :: Element a
    } deriving (Show, Eq)

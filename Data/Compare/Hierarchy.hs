module Hierarchy where

import Element

data Hierarchy a = Hierarchy (Element a) deriving (Show, Eq)

compareHier :: Hierarchy a -> Hierarchy a -> DifferenceSet a
compareHier = error "Not implemented"

compareHierWithAncestor ::
    Hierarchy a -> Hierarchy a -> Hierarchy a -> DifferenceSet a
compareHierWithAncestor = error "Not implemented"


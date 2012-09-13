module Content where

data Content a = FinalContent a
               | NestedContent [Element a] deriving (Show, Eq)

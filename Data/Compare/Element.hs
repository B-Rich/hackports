module Element where

import Content

data Element a = Element {
      moniker :: a
    , content :: Content
    } deriving (Show, Eq)


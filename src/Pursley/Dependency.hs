module Pursley.Dependency
  ( DependencyGraph,
    SortDependencyGraph (..),
  )
where

import Pursley.Prelude

type DependencyGraph a = Map a (Set a)

data SortDependencyGraph a = SortDependencyGraph
  { sortdepGraph :: DependencyGraph a,
    sortdepIndegrees :: Map a Int
  }
  deriving (Eq, Generic)

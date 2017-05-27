module Data.Zeppelin

  ( NamedDependency
  , Inflatable(..)
  , DependencyList(NilDeps)
  , HasDependencies(..)
  , SideLoaded(..)
  , inflate
  , getDependency
  , (&:)
  ) where

import Data.Zeppelin.Internal
import Data.Zeppelin.Swagger

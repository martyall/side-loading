{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilyDependencies #-}


module Data.SideLoaded where
--    ( NamedDependency(..)
--    , Deflated
--    , Inflatable(..)
--    , DependencyList(NilDeps)
--    , HasDependencies(..)
--    , SideLoaded
--    , inflate
--    , (&:)
--    ) where

import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Kind
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Data.Singletons (Apply, type (~>))
import Data.Text (Text, pack)
import Data.Type.List (Map, Map')
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)

--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: Inflatable m b f => b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

infixr 5 :&:

(&:) :: ( Monad m
        , Inflatable m b f
        )
     => b
     -> DependencyList m bs fs
     -> DependencyList m (b:bs) (f:fs)
(&:) b rest = b :&: rest

type family NamedDependency (a :: Type) :: Symbol

class ToKeyValueList a where
  toKeyValueList :: a -> [(Text, Value)]

instance ToKeyValueList (DependencyList Identity '[] '[]) where
  toKeyValueList _ = []

-- | JSON instances

instance ( ToJSON d
         , KnownSymbol (NamedDependency d)
         , ToKeyValueList (DependencyList Identity ds ds)
         ) => ToKeyValueList (DependencyList Identity (d:ds) (d:ds)) where
  toKeyValueList (a :&: rest) =
    let k = pack . symbolVal $ Proxy @(NamedDependency d)
        v = toJSON a
    in (k, v) : toKeyValueList rest

instance ToKeyValueList (DependencyList Identity ds ds) => ToJSON (DependencyList Identity ds ds) where
  toJSON ds = object $ toKeyValueList ds

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

--type family Deflated full = base | base -> full
--
-- | Inflatable represents an entity which can be expanded inside of a
-- context @m@.
class Inflatable m base full | base m -> full where
  inflator :: base -> m full

instance Inflatable Identity base base where
  inflator = return

class HasDependencies m a full | a -> full where
  type DependencyBase a :: [*]
  getDependencies :: a -> DependencyList m (DependencyBase a) full

----------------------------------------------------------------------------------
---- | Side Loading
----------------------------------------------------------------------------------

sequenceDependencyList :: Monad m => DependencyList m bs fs -> m (DependencyList Identity fs fs)
sequenceDependencyList NilDeps = return NilDeps
sequenceDependencyList (b :&: rest) = do
  f <- inflator b
  fs <- sequenceDependencyList rest
  return (f :&: fs)

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

instance ( ToJSON (DependencyList Identity deps deps)
         , ToJSON a
         ) => ToJSON (SideLoaded a deps) where
  toJSON (SideLoaded _data deps) = object [ "data" .= toJSON _data
                                          , "dependencies" .= toJSON deps
                                          ]

inflate :: ( HasDependencies m a fs
           , bs ~ DependencyBase a
           , CanInflate m bs fs
           , Monad m
           )
        => a
        -> m (SideLoaded a fs)
inflate value =
  let dependencies = getDependencies value
  in sequenceDependencyList dependencies >>= \deps -> return $ SideLoaded value deps

----------------------------------------------------------------------------------
---- Type Families
----------------------------------------------------------------------------------

type family AllSatisfy (subjects :: [k]) (test :: (k ~> Constraint)) :: Constraint where
  AllSatisfy '[] test = ()
  AllSatisfy (subj : rest) test = (Apply test subj, AllSatisfy rest test)

type family CanInflate (m :: * -> *) (bs :: [*]) (fs :: [*]) :: Constraint where
  CanInflate m '[] '[] = ()
  CanInflate m (b:bs) (f:fs) = (Inflatable m b f, CanInflate m bs fs)

data Inflatable' :: m -> b -> (f ~> Constraint) where
  Inflatable' :: Inflatable' m b f

type instance Apply (Inflatable' m b) f = Inflatable m b f

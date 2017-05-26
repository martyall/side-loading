module Data.Zeppelin.Internal
    ( NamedDependency
    , Inflatable(..)
    , DependencyList(NilDeps)
    , HasDependencies(..)
    , SideLoaded(..)
    , AllSatisfy
    , inflate
    , getDependency
    , (&:)
    ) where

import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Kind
import Data.Proxy
import Data.Functor.Identity (Identity(..))
import Data.Singletons (Apply, type (~>))
import Data.Singletons.TypeLits
import Data.Text (Text, pack)

--------------------------------------------------------------------------------
-- | Dependency Lists
--------------------------------------------------------------------------------

data DependencyList :: (* -> *) -> [*] -> [*] -> * where
  NilDeps :: DependencyList m '[] '[]
  (:&:) :: b -> DependencyList m bs fs -> DependencyList m (b:bs) (f:fs)

(&:) :: ( Monad m
        , Inflatable m b f
        )
     => b
     -> DependencyList m bs fs
     -> DependencyList m (b:bs) (f:fs)
(&:) b rest = b :&: rest

infixr 5 &:

-- | Labels for the objects created in the dependency mapping. Useful for JSON instances.
type family NamedDependency (a :: Type) :: Symbol

class ProjectDependency bs b where
  projectDependency' :: forall fs m . DependencyList m bs fs -> b

instance {-# OVERLAPPING #-} ProjectDependency (b : bs) b where
  projectDependency' (b :&: _) = b

instance {-# OVERLAPPABLE #-} ProjectDependency bs b =>  ProjectDependency (a : bs) b where
  projectDependency' (a :&: bs ) = projectDependency' bs

--------------------------------------------------------------------------------
-- | Inflatables
--------------------------------------------------------------------------------

-- | Inflatable represents an entity which can be expanded inside of a context @m@.
class Inflatable m base full | base m -> full where
  inflator :: base -> m full

-- | Anything can be expanded into itself in the trivial context
instance Inflatable Identity base base where
  inflator = return

-- | Indicate that a type has dependencies, and supply the uninflated types
-- (order matters here).
class HasDependencies m a fs | a -> fs where
  type DependencyBase a :: [*]
  getDependencies :: a -> DependencyList m (DependencyBase a) fs

----------------------------------------------------------------------------------
---- | Side Loading
----------------------------------------------------------------------------------

-- | Run the inflator in a monadic sequence.
sequenceDependencyList :: ( Monad m
                          , CanInflate m bs fs
                          )
                       => DependencyList m bs fs
                       -> m (DependencyList Identity fs fs)
sequenceDependencyList NilDeps = return NilDeps
sequenceDependencyList (b :&: rest) = do
  f <- inflator b
  fs <- sequenceDependencyList rest
  return (f :&: fs)

data SideLoaded a (deps :: [*]) = SideLoaded a (DependencyList Identity deps deps)

-- | Run the inflators and wrap in the SideLoaded type.
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

getDependency :: ProjectDependency deps dep
              => proxy dep
              -> SideLoaded a deps
              -> dep
getDependency _ (SideLoaded _ deps) = projectDependency' deps

--------------------------------------------------------------------------------
-- | JSON Instances
--------------------------------------------------------------------------------

class ToKeyValueList a where
  toKeyValueList :: a -> [(Text, Value)]

instance ToKeyValueList (DependencyList Identity '[] '[]) where
  toKeyValueList _ = []

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

instance {-# OVERLAPPABLE #-}
         ( ToJSON (DependencyList Identity deps deps)
         , ToJSON a
         ) => ToJSON (SideLoaded a deps) where
  toJSON (SideLoaded _data deps) = object [ "data" .= toJSON _data
                                          , "dependencies" .= toJSON deps
                                          ]

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

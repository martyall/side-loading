module Data.Zeppelin.Swagger where

import Control.Lens (mapped, (%~), (.~), (?~), (&), (^.), (^?))
import Data.Aeson (ToJSON(..), Value(Object), object, (.=))
import Data.Monoid ((<>))
import Control.Monad
import qualified Data.HashMap.Strict.InsOrd as O (insert, member, empty, fromList)
import qualified Data.HashMap.Lazy as U (HashMap, insert)
import Data.Kind
import Data.Proxy
import Data.Singletons.TypeLits
import Data.Swagger
import Data.Swagger.Declare
import Data.Text as T
import Data.Zeppelin.Internal

--------------------------------------------------------------------------------

-- | ToDependencySchema is just a proxy for ToSchema for our heterogenous list of dependencies.
class ToDependencySchema (deps :: [*]) where
  declareDependencySchema :: proxy deps -> Declare (Definitions Schema) NamedSchema

-- | base case.
instance ToDependencySchema '[] where
  declareDependencySchema _ =
    return $ NamedSchema Nothing
      ( mempty
        & type_ .~ SwaggerObject
        & properties .~ O.empty
        & example ?~ Object mempty
      )

-- | inductive step
instance ( ToDependencySchema deps
         , KnownSymbol (NamedDependency d)
         , ToSchema d
         , ToJSON d
         ) => ToDependencySchema (d : deps) where
  declareDependencySchema _ = do
    dRef <- declareSchemaRef $ Proxy @d
    let dExample = toSchema (Proxy @d) ^. example
        dText = T.pack . symbolVal $ Proxy @(NamedDependency d)
    declareDependencySchema (Proxy  :: Proxy deps)
      & mapped.schema.properties %~ O.insert dText dRef
      & mapped.schema.example %~ updateExample dText dExample
    where updateExample :: Text -> Maybe Value -> (Maybe Value -> Maybe Value)
          updateExample k mv = \ex -> case mv of
            Nothing -> ex
            Just v -> case ex of
              (Just (Object hm)) -> Just . Object $ U.insert k v hm
              -- this actually can't happen.
              _ -> Nothing

instance ( ToSchema a
         , ToDependencySchema deps
         , ToJSON a
         ) => ToSchema (SideLoaded a deps) where
  declareNamedSchema _ = do
    aRef <- declareSchemaRef $ Proxy @a
    depsRef <- declareDependencySchemaRef $ Proxy @deps
    depsNamedSchema <- declareDependencySchema (Proxy @deps)
    let aName = schemaName $ Proxy @a
        aExample = toSchema (Proxy @a) ^. example
        dExample = depsNamedSchema ^. schema . example
    return $ NamedSchema (fmap ("side-loaded JSON: " <>) aName)
      ( mempty
        & type_ .~ SwaggerObject
        & properties .~ O.fromList [("data", aRef), ("dependencies", depsRef)]
        & example .~ mkExample aExample dExample
      )
    where
      mkExample :: Maybe Value -> Maybe Value -> Maybe Value
      mkExample me1 me2 = do
        e1 <- me1
        e2 <- me2
        return $ object [ "data" .= e1
                        , "dependencies" .= e2
                        ]

--  | PolyKinded version of declareSchemaRef.
declareDependencySchemaRef :: ToDependencySchema deps
                           => proxy deps
                           -> Declare (Definitions Schema) (Referenced Schema)
declareDependencySchemaRef deps = do
  case undeclare . declareDependencySchema $ deps of
    NamedSchema (Just name) schema -> do
      known <- looks (O.member name)
      when (not known) $ do
        declare $ O.fromList [(name, schema)]
        void $ declareDependencySchema deps
      return $ Ref (Reference name)
    _ -> Inline <$> (fmap _namedSchemaSchema $ declareDependencySchema deps)

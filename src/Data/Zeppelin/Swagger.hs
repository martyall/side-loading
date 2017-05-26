module Data.Zeppelin.Swagger where

import Control.Lens (mapped, (%~), (&))
import Data.Monoid ((<>))
import Data.Proxy
import Data.Swagger
import Data.Zeppelin.Internal (SideLoaded(..))

instance ToSchema a => ToSchema (SideLoaded a deps) where
  declareNamedSchema _ = declareNamedSchema (Proxy @a) &
    mapped.schema.description.mapped %~ (<> " This is side-loaded with additional data in the dependencies.")

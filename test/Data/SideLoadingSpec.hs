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
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Data.SideLoadingSpec (spec) where

import Data.SideLoaded
import Data.Aeson (ToJSON(..), encode)
import GHC.Generics (Generic)
import GHC.TypeLits
import Test.Hspec


spec :: Spec
spec = do
  describe "it should inflate albums" $ do
    it "can print the correct result" $ do
      a <- inflate album
      print . encode $ toJSON a
      return ()

--------------------------------------------------------------------------------
-- | Photo

newtype PhotoId = PhotoId Int deriving (Eq, Show, Num, ToJSON)

type instance NamedDependency [Photo] = "photos"

data Photo =
  Photo { photoId :: PhotoId
        , photoCaption :: String
        , photoAlbumId :: AlbumId
        , artistId :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo

photos = [Photo 1 "At the Beach" 1 1, Photo 2 "In the Mountains" 1 1]

instance Inflatable IO [PhotoId] [Photo] where
  inflator = const $ return photos

-- | Person

newtype PersonId = PersonId Int deriving (Eq, Show, Num, ToJSON)

type instance NamedDependency Person = "person"

data Person =
  Person { personId :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person

john :: Person
john = Person 1 "Johnathon"

instance Inflatable IO PersonId Person where
  inflator = const $ return john

-- | Albums

newtype AlbumId = AlbumId Int deriving (Eq, Show, Num, ToJSON)

data Album =
  Album { albumId :: AlbumId
        , albumName :: String
        , albumOwner :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album

instance HasDependencies IO Album [Person, [Photo]] where
  type DependencyBase Album = [PersonId, [PhotoId]]
  getDependencies (Album _ _ owner pIds) = owner &: (pIds &: NilDeps)

album :: Album
album = Album 1 "Vacations" 1 [1,2]

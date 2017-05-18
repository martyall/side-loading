{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
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
import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), encode, decode, (.:))
import Data.Aeson.Types (Parser)
import Data.IORef
import Data.Maybe (isJust)
import Data.Proxy
import GHC.Generics (Generic)
import GHC.TypeLits
import System.IO.Unsafe (unsafePerformIO)
import Test.Hspec

import Debug.Trace

spec :: Spec
spec = do

  describe "it should inflate albums" $ do
    it "lazily inflates" $ do
      _ <- writeIORef photosInflationCount 0
      _ <- getDependency (Proxy @ Person) <$> inflate album
      n <- readCount photosInflationCount
      n `shouldBe` 0

    it "can serialize dependencies" $ do
      serializedAlbum <- fmap (encode . toJSON) . inflate $ album
      let (mpair :: Maybe (Person, [Photo])) = decode serializedAlbum
      mpair `shouldSatisfy` isJust
      let (Just(o, ps)) = mpair
      o `shouldBe` john
      ps `shouldBe` photos


    it "can do projections" $ do
      inflatedAlbum <- inflate $ album
      getDependency (Proxy @ Person) inflatedAlbum `shouldBe` john

    it "can do some traversals" $ do
      inflatedAlbum <- inflate $ album
      let ps = getDependency (Proxy @ [Photo]) inflatedAlbum
      length ps `shouldSatisfy` (>= 1)
      let (p:rest) = ps
      inflatedPhoto <- inflate p
      getDependency (Proxy @ Person) inflatedPhoto `shouldBe` john

--------------------------------------------------------------------------------
-- | Person

newtype PersonId = PersonId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

type instance NamedDependency Person = "person"

data Person =
  Person { personId :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person
instance FromJSON Person

john :: Person
john =  Person 1 "John"

john' :: IORef Person
john' = unsafePerformIO $ newIORef john
{-# NOINLINE john #-}

instance Inflatable IO PersonId Person where
  inflator = const $ traceShowId <$> readIORef john'

-- | Photo

newtype PhotoId = PhotoId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

type instance NamedDependency [Photo] = "photos"

data Photo =
  Photo { photoId :: PhotoId
        , photoCaption :: String
        , photoAlbumId :: AlbumId
        , artistId :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo
instance FromJSON Photo

photos :: [Photo]
photos = [Photo 1 "At the Beach" 1 1, Photo 2 "In the Mountains" 1 1]

photos' :: IORef [Photo]
photos' = unsafePerformIO $ newIORef photos
{-# NOINLINE photos #-}

photosInflationCount :: IORef Int
photosInflationCount = unsafePerformIO $ newIORef 0
{-# NOINLINE photosInflationCount #-}

tic :: IORef Int -> IORef a -> IO a
tic counter ref = modifyIORef counter (+1) >> readIORef ref

readCount :: IORef Int -> IO Int
readCount = readIORef

instance Inflatable IO [PhotoId] [Photo] where
  inflator = const $ tic photosInflationCount photos'

instance HasDependencies IO Photo '[Album, Person] where
  type DependencyBase Photo = '[AlbumId, PersonId]
  getDependencies (Photo _ _ aId pId) = aId &: pId &: NilDeps

-- | Albums

newtype AlbumId = AlbumId Int deriving (Eq, Show, Num, ToJSON, FromJSON)

data Album =
  Album { albumId :: AlbumId
        , albumName :: String
        , albumOwner :: PersonId
        , albumPhotos :: [PhotoId]
        } deriving (Eq, Show, Generic)

instance ToJSON Album

instance HasDependencies IO Album [Person, [Photo]] where
  type DependencyBase Album = [PersonId, [PhotoId]]
  getDependencies (Album _ _ owner pIds) = owner &: pIds &: NilDeps

album :: Album
album = Album 1 "Vacations" 1 [1,2]

album' :: IORef Album
album' = unsafePerformIO $ newIORef album
{-# NOINLINE album' #-}

instance Inflatable IO AlbumId Album where
  inflator = const $ readIORef album'

instance {-# OVERLAPPING #-} FromJSON (Person, [Photo]) where
  parseJSON (Object o) =
    (,) <$> ((o .: "dependencies") >>= (.: "person"))
        <*> ((o .: "dependencies") >>= (.: "photos"))
  parseJSON invalid = fail "could not parse dependencies"

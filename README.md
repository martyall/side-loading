# side-loading

```haskell

-- | Person

newtype PersonId = PersonId Int deriving (Eq, Show, Num, ToJSON)

data Person =
  Person { personId :: PersonId
         , personName :: String
         } deriving (Eq, Show, Generic)

instance ToJSON Person

type instance NamedDependency Person = "person"

instance Inflatable IO PersonId Person where
  inflator = const $ return john

-- | Photos

newtype PhotoId = PhotoId Int deriving (Eq, Show, Num, ToJSON)


data Photo =
  Photo { photoId :: PhotoId
        , photoCaption :: String
        , photoAlbumId :: AlbumId
        , artistId :: PersonId
        } deriving (Eq, Show, Generic)

instance ToJSON Photo

type instance NamedDependency [Photo] = "photos"

instance Inflatable IO [PhotoId] [Photo] where
  inflator = const $ return photos

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
  getDependencies (Album _ _ owner pIds) = owner &: pIds &: NilDeps

-- | instantiations

john :: Person
john = Person 1 "Johnathon"

photos :: [Photo]
photos = [Photo 1 "At the Beach" 1 1, Photo 2 "In the Mountains" 1 1]

album :: Album
album = Album 1 "Vacations" 1 [1,2]

```

Then the `toJSON . inflate $ album` gives us

```json
{
  "data": {
    "albumId": 1,
    "albumPhotos": [
      1,
      2
    ],
    "albumName": "Vacations",
    "albumOwner": 1
  },
  "dependencies": {
    "person": {
      "personName": "John",
      "personId": 1
    },
    "photos": [
      {
        "artistId": 1,
        "photoCaption": "At the Beach",
        "photoAlbumId": 1,
        "photoId": 1
      },
      {
        "artistId": 1,
        "photoCaption": In the Mountains",
        "photoAlbumId": 1,
        "photoId": 2
      }
    ]
  }
}

```

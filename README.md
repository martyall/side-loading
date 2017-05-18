# side-loading

[Introduction](#introduction)
[Example](#example)
[Traversals](#traversals)

## Introduction
This is a _really_ small library for doing server side loading for JSON using `Aeson`. The use case is as follows: Suppose you have a datatype like `Album` that _depends on_ a bunch of `Photo`s. By "depends on", we mean something like `Album` is a dataype with a field of type `[PhotoId]` which is the list of ids of all of the photos in the album. For example:

```haskell
data Album =
  Album { albumId :: AlbumId
        , albumName :: String
        , albumOwner :: PersonId
        , albumPhotos :: [PhotoId]
        }
```

Now if the server gets a request at `site/albums/:albumId`, we might expect a response like

```json
{
  "albumId": 1,
  "albumPhotos": [
    1,
    2
  ],
  "albumName": "Vacations",
  "albumOwner": 1
}
```

The problem is that this response is basically useless from a frontend's point of view. We would then need to make a batch request to `/site/album/1/photos` to pull the data for the photos, and a request to `/site/people/1` to get info about the owner of the photo album. What if we needed to display all of this data now? Shouldn't we be able to pull enough of the dependencies on the first request to do something useful?

`side-loaded` aims to solve this problem without writing a bunch of boilerplate. It has a small number of type classes that you must implement in order to get started, and in theory you should not have to write any new code. Take the above example of photo `Album`s, `Photo`s, and `Person`s. You should already have basic queries to read datatypes from a database,
say

```haskell
-- DB is a monad where we can make database queries.
newtype DB a = ... 

getPhotos :: [PhotoId] -> DB [Photo]
getPhotos = ...

getPerson :: PersonId -> DB Person
getPerson = ...
```

We say that `Photo` and `Person` are _inflatable_, meaning it's possible to start with a skeletal piece of data like `PhotoId` and inflate this to the full datatype `Photo`. To make this precise, you would implement the following type class

```haskell
instance Inflatable DB [PhotoId] [Photo] where
  inflator = getPhotos
  
instance Inflatable DB PersonId Person where
  inflator = getPerson
```

To indicate that `Album` could be side loaded with data about `Photos` and `Person`s, we implement the `HasDependencies` type class:

```haskell
instance HasDependencies Album DB [Person, [Photo]] where
  type DependenciesBase Album = [PersonId, [PhotoId]]
  getDependencies (Album _ _ owner photoIds) = owner &: photoIds &: NilDeps

```
Through some type level magic (including a functional dependency on the `Inflatable` type class) you're telling the compiler that we're going to inflate `PersonId` into a `Person` inside the `DB` context and there's only one way to do that which is via the `Inflatable` instance provided, similarly with `[PhotoId]` to `Photo`. You then have access to a function

```haskell
inflate :: Album -> SideLoaded Album [Person, [Photo]]
```
which will grab all of this data and fill it in for you. 

## Example
Here is our toy example written out in full with a mocked database (you can compile and run this):

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
john = Person 1 "John"

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
        "photoCaption": "In the Mountains",
        "photoAlbumId": 1,
        "photoId": 2
      }
    ]
  }
}

```

Do you also need versioning? If so check out [aeson-versioned](https://github.com/benweitzman/servant-aeson-versioned) and [aeson-versioned-sideloading](https://github.com/benweitzman/aeson-versioned-sideloading)

## Traversals
We also supply a polymorphic accessor `getDependency` to get the dependencies out of the `Sideloaded` type, for example

```haskell
getAlbumOwner :: Album -> Person 
getAlbumOwner album = do
inflatedAlbum <- inflate album
owner <- getDependency (Proxy @ Person) inflatedAlbum
return owner
```

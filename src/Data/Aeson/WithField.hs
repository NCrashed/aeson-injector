{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-|
Module      : Data.Aeson.WithField
Description : Provides utility to inject fields into aeson values.
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

When builds a RESTful API one often faces the problem that some methods
need inbound data without unique identifier (for instance, a creation of
new resource) and some methods need the same outbound data with additional
fields attached to the response.

The module provides you with 'WithField' and 'WithFields' data types that
help you to solve the issue without code duplication.

It is small utility library that is intented to be used in RESTful APIs,
especially with <http://haskell-servant.readthedocs.io/en/stable/ servant>
and <http://swagger.io/ Swagger>. Its main purpose is simple injection of
fields into JSONs produced by <https://hackage.haskell.org/package/aeson aeson>
library.

Consider the following common data type in web service developing:

@
data News = News {
  title :: Text
, body :: Text
, author :: Text
, timestamp :: UTCTime
}

-- Consider we have simple 'ToJSON' and 'FromJSON' instances
$(deriveJSON defaultOptions ''News)
@

'ToJSON' instance produces JSON's like:

@
{
  "title": "Awesome piece of news!"
, "body": "Big chunk of text"
, "author": "Just Me"
, "timestamp": "2016-07-26T18:54:42.678999Z"
}
@

Now one can create a simple web server with servant DSL:

> type NewsId = Word
>
> type NewsAPI =
>        ReqBody '[JSON] News :> Post '[JSON] NewsId
>   :<|> Capture "news-id" NewsId :> Get '[JSON] News
>   :<|> "list" :> Get '[JSON] [News]


All seems legit, but, wait a second, an API user definitely would
like to know id of news in the "list" method. One way to do this is declare
new data type @NewsInfo@ with additional field, but it is bad solution as requires
to code duplication for each resource.

So, here @aeson-injector@ steps in, now you can write:

> type NewsAPI =
>        ReqBody '[JSON] News :> Post '[JSON] NewsId
>   :<|> Capture "news-id" NewsId :> Get '[JSON] News
>   :<|> "list" :> Get '[JSON] [WithField "id" NewsId News]

@'WithField' "id" NewsId News@ or simply @'WithId' NewsId News@ wraps you data type
and injects "id" field in produced JSON values:

>>> encode (WithField 42 myNews :: WithField "id" NewsId News)

> {
>   "id": 42
> , "title": "Awesome piece of news!"
> , "body": "Big chunk of text"
> , "author": "Just Me"
> , "timestamp": "2016-07-26T18:54:42.678999Z"
> }

'WithField' data type has `FromJSON` instance for seamless parsing of data with
injected fields and 'ToSchema' instance for <https://hackage.haskell.org/package/servant-swagger servant-swagger> support.

= Injecting multiple values

The library also has more general data type 'WithFields a b' that injects fields of 'toJSON a' into 'toJSON b'.

@ haskell
data NewsPatch = NewsPatch {
  taggs :: [Text]
, rating :: Double
}
$(deriveJSON defaultOptions ''NewsPatch)
@

@ haskell
let myNewsPatch = NewsPatch ["tag1", "tag2"] 42
in encode $ WithFields myNewsPatch myNews
@

> {
>   "title": "Awesome piece of news!"
> , "body": "Big chunk of text"
> , "author": "Just Me"
> , "timestamp": "2016-07-26T18:54:42.678999Z"
> , "tags": ["tag1", "tag2"]
> , "rating": 42.0
> }

= Corner cases

Unfortunately, we cannot inject in non object values of produced JSON,
so the library creates a wrapper object around non-object value:

@
encode (WithId 0 "non-object" :: WithId Int String)
@

@
{
  "id": 0
, "value": "non-object"
}
@

The same story is about 'WithFields' data type:

@
encode (WithFields 0 "non-object" :: WithFields Int String)
@

@
{
  "injected": 0
, "value": "non-object"
}
@

-}
module Data.Aeson.WithField(
  -- * Single field injector
    WithField(..)
  , WithId
  -- * Multiple fields injector
  , WithFields(..)
  -- * Single field wrapper
  , OnlyField(..)
  , OnlyId
  ) where

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad
import Data.Aeson
import Data.Aeson.Types (typeMismatch)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.WithField.Internal
import Data.Hashable
import Data.Monoid
import Data.Proxy
import Data.Swagger
import GHC.Generics
import GHC.TypeLits
import Servant.Docs

import qualified Data.Foldable as F
import qualified Data.List as L
import qualified Data.Text as T

-- | Injects field 'a' into 'b' with tag 's'. It has
-- special instances for 'ToJSON' and 'FromJSON' for
-- such injection and corresponding Swagger 'ToSchema'
-- instance.
--
-- For instance:
--
-- >>> encode (WithField "val" (Left 42) :: WithField "injected" String (Either Int Int))
-- "{\"Left\":42,\"id\":\"val\"}"
--
-- If the instance cannot inject field (in case of single values and arrays),
-- it wraps the result in the following way:
--
-- >>> encode (WithField "val" 42 :: WithField "injected" String Int)
-- "{\"value\":42,\"injected\":\"val\"}"
--
-- `WithField s a b` always overwites field `s` in JSON produced by `b`.
data WithField (s :: Symbol) a b = WithField !a !b
  deriving (Generic, Eq, Show, Read)

instance (NFData a, NFData b) => NFData (WithField s a b)

instance Functor (WithField s a) where
  fmap f (WithField a b) = WithField a (f b)

instance Bifunctor (WithField s) where
  bimap fa fb (WithField a b) = WithField (fa a) (fb b)

-- | Workaround for a problem that is discribed as:
-- sometimes I need a id with the data, sometimes not.
--
-- The important note that 'ToJSON' and 'FromJSON' instances
-- behaves as it is 'a' but with additional 'id' field.
type WithId i a = WithField "id" i a

instance (ToSample a, ToSample b) => ToSample (WithField s a b) where
  toSamples _ = samples $ WithField <$> as <*> bs
    where
    as = snd <$> toSamples (Proxy :: Proxy a)
    bs = snd <$> toSamples (Proxy :: Proxy b)

-- | Note: the instance injects field only in 'Object' case.
-- In other cases it forms a wrapper around the 'Value' produced
-- by 'toJSON' of inner 'b' body.
--
-- Example of wrapper:
--
-- > { "id": 0, "value": [1, 2, 3] }
instance (KnownSymbol s, ToJSON a, ToJSON b) => ToJSON (WithField s a b) where
  toJSON (WithField a b) = let
    jsonb = toJSON b
    field = mkFieldName @s
    in case toJSON b of
      Object vs -> Object $ KM.insert field (toJSON a) vs
      _ -> object [
          "value" .= jsonb
        , field .= a
        ]

-- | Note: the instance tries to parse the json as object with
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
--
-- Note: The instance tries to parse the `b` part without `s` field at first time.
-- If it fails, the instance retries with presence of the `s` field.
instance (KnownSymbol s, FromJSON a, FromJSON b) => FromJSON (WithField s a b) where
  parseJSON val@(Object o) = injected `mplus0` wrapper
    where
    field = mkFieldName @s
    injected = WithField
      <$> o .: field
      <*> (parseJSON (Object $ KM.delete field o) <|> parseJSON val)
    wrapper = WithField
      <$> o .: field
      <*> o .: "value"
  parseJSON wat = typeMismatch "Expected JSON Object" wat

-- | Note: the instance tries to generate schema of the json as object with
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (KnownSymbol s, ToSchema a, ToSchema b) => ToSchema (WithField s a b) where
  declareNamedSchema _ = do
    NamedSchema n s <- declareNamedSchema (Proxy :: Proxy b)
    if s ^. type_ == Just SwaggerObject then inline n s
      else wrapper n s
    where
    field = T.pack $ symbolVal (Proxy :: Proxy s)
    namePrefix = "WithField '" <> field <> "' "
    wrapper n s = do
      indexSchema <- declareSchema (Proxy :: Proxy a)
      return $ NamedSchema (fmap (namePrefix <>) n) $ mempty
        & type_ .~ Just SwaggerObject
        & properties .~
            [ ("value", Inline s)
            , (field, Inline indexSchema)
            ]
        & required .~ (L.nub [ "value", field ])
    inline n s = do
      indexSchema <- declareSchema (Proxy :: Proxy a)
      return $ NamedSchema (fmap (namePrefix <>) n) $ s
        & properties %~ ([(field, Inline indexSchema)] <>)
        & required %~ ([field] <>)

-- | Merge fields of 'a' into 'b', more general version of 'WithField'.
--
-- The usual mode of the data type assumes that 'ToJSON' instances of 'a' and 'b'
-- produce 'Object' subtype of aeson 'Value'. If it is not true, a wrapper
-- layer is introduced.
--
-- If 'a' is not a 'Object', the wrapper contains 'injected' field with body of 'a'.
-- If 'b' is not a 'Object', the wrapper contains 'value' field with body of 'b'.
-- If both are not 'Object', the wrapper contains 'injected' and 'value' keys with
-- 'a' and 'b' respectively.
data WithFields a b = WithFields !a !b
  deriving (Generic, Eq, Show, Read)

instance (NFData a, NFData b) => NFData (WithFields a b)

instance Functor (WithFields a) where
  fmap f (WithFields a b) = WithFields a (f b)

instance Bifunctor WithFields where
  bimap fa fb (WithFields a b) = WithFields (fa a) (fb b)

instance (ToSample a, ToSample b) => ToSample (WithFields a b) where
  toSamples _ = samples $ WithFields <$> as <*> bs
    where
    as = snd <$> toSamples (Proxy :: Proxy a)
    bs = snd <$> toSamples (Proxy :: Proxy b)

-- | Note: the instance injects field only in 'Object' case.
-- In other cases it forms a wrapper around the 'Value' produced
-- by 'toJSON' of inner 'b' body.
--
-- Example of wrapper when 'b' is not a 'Object', 'b' goes into "value" field:
--
-- > { "field1": 0, "field2": "val", "value": [1, 2, 3] }
--
-- Example of wrapper when 'a' is not a 'Object', but 'b' is. 'a' goes into
-- "injected" field:
--
-- > { "field1": 0, "field2": "val", "injected": [1, 2, 3] }
--
-- Example of wrapper when as 'a' is not a 'Object', as 'b' is not. 'a' goes into
-- "injected" field, 'b' goes into "value" field:
--
-- > { "value": 42, "injected": [1, 2, 3] }
--
-- `WithFields a b` always overwites fields in JSON produced by `b` with fields from JSON
-- produced by `a`.
instance (ToJSON a, ToJSON b) => ToJSON (WithFields a b) where
  toJSON (WithFields a b) = let
    jsonb = toJSON b
    jsona = toJSON a
    in case jsonb of
      Object bvs -> case jsona of
        Object avs -> Object $ KM.union avs bvs
        _ -> Object $ KM.insert "injected" jsona bvs
      _ -> case jsona of
        Object avs -> Object $ case KM.lookup "value" avs of
          Nothing -> KM.insert "value" jsonb avs
          Just _ -> avs
        _ -> object [
            "injected" .= jsona
          , "value" .= jsonb
          ]

-- | Note: the instance tries to parse the json as object with
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
--
-- Note: The instance tries to parse the `b` part without fields of `a` at first time.
-- If it fails, the instance retries with presence of a's fields.
--
-- The implementation requires `ToJSON a` to catch fields of `a` and it is assumed
-- that `fromJSON . toJSON === id` for `a`.
instance (ToJSON a, FromJSON a, FromJSON b) => FromJSON (WithFields a b) where
  parseJSON val@(Object o) = do
    (a, isInjected) <- ((, False) <$> parseJSON val) `mplus0` ((, True) <$> (o .: "injected"))
    let o' = (if isInjected then KM.delete "injected" else deleteAll (extractFields a)) o
    b <-  ((parseJSON (Object o')) `mplus0` (o' .: "value"))
      <|> ((parseJSON val) `mplus0` (o .: "value"))
    pure $ WithFields a b
    where
      deleteAll :: [Key.Key] -> KM.KeyMap v -> KM.KeyMap v
      deleteAll ks m = F.foldl' (flip KM.delete) m ks

      extractFields :: ToJSON a => a -> [Key.Key]
      extractFields a = case toJSON a of
        Object vs -> KM.keys vs
        _ -> []
  parseJSON wat = typeMismatch "Expected JSON Object" wat

-- | Note: the instance tries to generate schema of the json as object with
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (ToSchema a, ToSchema b) => ToSchema (WithFields a b) where
  declareNamedSchema _ = do
    NamedSchema nb sb <- declareNamedSchema (Proxy :: Proxy b)
    NamedSchema na sa <- declareNamedSchema (Proxy :: Proxy a)
    let newName = combinedName <$> na <*> nb
    return . NamedSchema newName $ case (sa ^. type_ , sb ^. type_) of
      (Just SwaggerObject, Just SwaggerObject) -> sb <> sa
      (Just SwaggerObject, _) -> bwrapper sb <> sa
      (_, Just SwaggerObject) -> sb <> awrapper sa
      _ -> bwrapper sb <> awrapper sa
    where
    combinedName a b = "WithFields_" <> a <> "_" <> b
    awrapper nas = mempty
      & type_ .~ Just SwaggerObject
      & properties .~ [ ("injected", Inline nas) ]
      & required .~ [ "injected" ]
    bwrapper nbs = mempty
      & type_ .~ Just SwaggerObject
      & properties .~ [ ("value", Inline nbs) ]
      & required .~ [ "value" ]

-- | Special case, when you want to wrap your type @a@ in field with name @s@.
--
-- >>> encode (OnlyField 0 :: OnlyField "id" Int)
-- "{\"id\":0}"
--
-- >>> encode $ toSchema (Proxy :: Proxy (OnlyField "id" Int))
-- "{\"required\":[\"id\"],\"type\":\"object\",\"properties\":{\"id\":{\"maximum\":9223372036854775807,\"minimum\":-9223372036854775808,\"type\":\"integer\"}}}"
--
-- Also the type can be used as an endpoint for 'WithField':
--
-- >>> encode (WithField True (OnlyField 0) :: WithField "val" Bool (OnlyField "id" Int))
-- "{\"id\":0,\"val\":true}"
newtype OnlyField (s :: Symbol) a = OnlyField { unOnlyField :: a }
  deriving (Generic, Show, Read, Eq)

-- | Special case for the most common "id" field
type OnlyId i = OnlyField "id" i

instance Functor (OnlyField s) where
  fmap f (OnlyField a) = OnlyField (f a)

instance ToSample a => ToSample (OnlyField s a) where
  toSamples _ = samples $ OnlyField <$> as
    where
    as = snd <$> toSamples (Proxy :: Proxy a)

instance (KnownSymbol s, ToJSON a) => ToJSON (OnlyField s a) where
  toJSON (OnlyField a) = object [ mkFieldName @s .= a ]

instance (KnownSymbol s, FromJSON a) => FromJSON (OnlyField s a) where
  parseJSON (Object o) = OnlyField <$> o .: (mkFieldName @s)
  parseJSON _ = mzero

instance (KnownSymbol s, ToSchema a) => ToSchema (OnlyField s a) where
  declareNamedSchema _ = do
    NamedSchema an as <- declareNamedSchema (Proxy :: Proxy a)
    let namePrefix = "OnlyField '" <> Key.toText field <> "' "
    return $ NamedSchema (fmap (namePrefix <>) an) $ mempty
      & type_ .~ Just SwaggerObject
      & properties .~ [(Key.toText field, Inline as)]
      & required .~ [Key.toText field]
    where
    field = mkFieldName @s

mkFieldName :: forall s . KnownSymbol s => Key.Key
mkFieldName = Key.fromString $ symbolVal (Proxy :: Proxy s)


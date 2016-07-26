{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
-}
module Data.Aeson.WithField(
  -- * Single field injector
    WithField(..)
  , WithId
  -- * Multiple fields injector
  , WithFields(..)
  ) where 

import Control.Applicative
import Control.DeepSeq
import Control.Lens hiding ((.=))
import Control.Monad 
import Data.Aeson 
import Data.Monoid 
import Data.Proxy 
import Data.Swagger 
import GHC.Generics 
import GHC.TypeLits 

import qualified Data.HashMap.Strict as H 
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
data WithField (s :: Symbol) a b = WithField !a !b 
  deriving (Generic, Eq, Show, Read)

instance (NFData a, NFData b) => NFData (WithField s a b)

-- | Workaround for a problem that is discribed as:
-- sometimes I need a id with the data, sometimes not.
--
-- The important note that 'ToJSON' and 'FromJSON' instances
-- behaves as it is 'a' but with additional 'id' field.
type WithId i a = WithField "id" i a 

-- | Note: the instance injects field only in 'Object' case.
-- In other cases it forms a wrapper around the 'Value' produced 
-- by 'toJSON' of inner 'b' body.
--
-- Example of wrapper:
--
-- > { "id": 0, "value": [1, 2, 3] }
instance (KnownSymbol s, ToJSON a, ToJSON b) => ToJSON (WithField s a b) where 
  toJSON (WithField a b) = let
    json = toJSON b 
    field = T.pack $ symbolVal (Proxy :: Proxy s)
    in case toJSON b of 
      Object vs -> Object $ H.insert field (toJSON a) vs 
      _ -> object [
          field .= a 
        , "value" .= json
        ]

-- | Note: the instance tries to parse the json as object with 
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (KnownSymbol s, FromJSON a, FromJSON b) => FromJSON (WithField s a b) where 
  parseJSON json@(Object o) = injected <|> wrapper 
    where 
    field = T.pack $ symbolVal (Proxy :: Proxy s)
    injected = WithField 
      <$> o .: field
      <*> parseJSON json
    wrapper = WithField 
      <$> o .: field
      <*> o .: "value"
  parseJSON _ = mzero

-- | Note: the instance tries to generate schema of the json as object with 
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (KnownSymbol s, ToSchema a, ToSchema b) => ToSchema (WithField s a b) where 
  declareNamedSchema prx = do 
    NamedSchema n s <- declareNamedSchema (Proxy :: Proxy b)
    if s ^. type_ == SwaggerObject then inline n s 
      else wrapper n s
    where 
    field = T.pack $ symbolVal (Proxy :: Proxy s)
    namePrefix = "WithField '" <> field <> "' "
    wrapper n s = do 
      indexSchema <- declareSchema (Proxy :: Proxy a)
      return $ NamedSchema (fmap (namePrefix <>) n) $ mempty
        & type_ .~ SwaggerObject
        & properties .~
            [ (field, Inline indexSchema)
            , ("value", Inline s)
            ]
        & required .~ [ field, "value" ]
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
instance (ToJSON a, ToJSON b) => ToJSON (WithFields a b) where 
  toJSON (WithFields a b) = let
    jsonb = toJSON b 
    jsona = toJSON a
    in case jsonb of 
      Object bvs -> case jsona of 
        Object avs -> Object $ H.union avs bvs
        _ -> Object $ H.insert "injected" jsona bvs 
      _ -> case jsona of 
        Object avs -> Object $ H.insert "value" jsonb avs
        _ -> object [
            "injected" .= jsona
          , "value" .= jsonb
          ]

-- | Note: the instance tries to parse the json as object with 
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (FromJSON a, FromJSON b) => FromJSON (WithFields a b) where 
  parseJSON json@(Object o) = WithFields
    <$> (parseJSON json <|> o .: "injected")
    <*> (parseJSON json <|> o .: "value")
  parseJSON _ = mzero

-- | Note: the instance tries to generate schema of the json as object with 
-- additional field value, if it fails it assumes that it is a
-- wrapper produced by corresponding 'ToJSON' instance.
instance (ToSchema a, ToSchema b) => ToSchema (WithFields a b) where 
  declareNamedSchema prx = do 
    nbs@(NamedSchema nb sb) <- declareNamedSchema (Proxy :: Proxy b)
    nas@(NamedSchema na sa) <- declareNamedSchema (Proxy :: Proxy a)
    let newName = combinedName <$> na <*> nb
    return . NamedSchema newName $ case (sa ^. type_ , sb ^. type_) of 
      (SwaggerObject, SwaggerObject) -> sa <> sb 
      (SwaggerObject, _) -> sa <> bwrapper sb
      (_, SwaggerObject) -> awrapper sa <> sb
      _ -> awrapper sa <> bwrapper sb
    where
    combinedName a b = "WithFields_" <> a <> "_" <> b
    awrapper nas = mempty
      & type_ .~ SwaggerObject
      & properties .~ [ ("injected", Inline nas) ]
      & required .~ [ "injected" ]
    bwrapper nbs = mempty
      & type_ .~ SwaggerObject
      & properties .~ [ ("value", Inline nbs) ]
      & required .~ [ "value" ]
  
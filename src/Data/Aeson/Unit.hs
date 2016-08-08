{-# LANGUAGE DeriveGeneric #-}
{-|
Module      : Data.Aeson.Unit
Description : Provides unit type that serialises into empty JSON object
Copyright   : (c) Anton Gushcha, 2016
License     : MIT
Maintainer  : ncrashed@gmail.com
Stability   : experimental
Portability : Portable

Common problem in REST interfaces when you need to return nothing as result, 
usage of `()` will produce `[]` JSON. That causes problems in some JSON parsers
in other languages. 

So, `Unit` serialises into empty JSON object:

>>> encode Unit
"{}"

-}
module Data.Aeson.Unit(
    Unit(..)
  ) where 

import Control.Lens
import Data.Aeson 
import Data.Swagger
import GHC.Generics 

-- | Data type that serialise into empty object in aeson
--
-- >>> encode Unit
-- "{}"
--
-- >>> encode $ toSchema (Proxy :: Proxy Unit)
-- "{\"type\":\"object\"}"
--
-- Also the 'FromJSON' instance is just `pure Unit`, so it never fails.
data Unit = Unit
  deriving (Generic, Eq, Show, Read, Enum, Bounded)

instance ToJSON Unit where 
  toJSON _ = object []

-- | Always a success parse 
instance FromJSON Unit where 
  parseJSON _ = pure Unit

instance ToSchema Unit where 
  declareNamedSchema _ = do 
    return $ NamedSchema Nothing $ mempty
      & type_ .~ SwaggerObject
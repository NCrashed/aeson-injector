{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Aeson.WithField.Internal
  ( mplus0
  ) where

import Data.Aeson.Types (Parser, JSONPath)

import Unsafe.Coerce

newtype P a = P 
   { runP :: forall f r.
                   JSONPath
                -> Failure f r
                -> Success a f r
                -> f r }

type Failure f r   = JSONPath -> String -> f r
type Success a f r = a -> f r

mplus0 :: forall a . Parser a -> Parser a -> Parser a
mplus0 a b = unsafeCoerce @(P a) $ P $ \path kf ks -> 
  let kf' p l = runP (unsafeCoerce b) path (\_ _ -> kf p l) ks
  in runP (unsafeCoerce a) path kf' ks

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Control.Lens hiding ((.=))
import Control.Monad 
import Data.Aeson
import Data.Aeson.WithField
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Text 
import Data.Proxy
import Test.HUnit 

main :: IO ()
main = runTestTT tests >> return ()
  where
  tests = TestList [
      (TestLabel "WithField tests" withFieldTests)
    , (TestLabel "WithFields tests" withFieldsTests)
    ]

data TestObj = TestObj !Text 
  deriving (Eq, Show)

instance ToJSON TestObj where 
  toJSON (TestObj t) = object ["field" .= t]
instance FromJSON TestObj where 
  parseJSON (Object o) = TestObj <$> o .: "field"
  parseJSON _ = mzero
instance ToSchema TestObj where 
  declareNamedSchema prx = do 
    t <- declareSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "TestObj") $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ [("field", Inline t)]
      & required .~ ["field"]

withFieldTests :: Test
withFieldTests = TestList [
    TestLabel "toJSON" testsToJSON
  , TestLabel "fromJSON" testsFromJSON
  , TestLabel "toSchema" testsToSchema
  ]
  where
  testsToJSON = TestList [
      TestLabel "Inline mode: atomic field" $ TestCase $ do 
        let expected = object ["a" .= (0 :: Int), "field" .= ("val" :: Text)]
        let actual = toJSON (WithField 0 (TestObj "val") :: WithField "a" Int TestObj)
        expected @=? actual
    , TestLabel "Inline mode: complex field" $ TestCase $ do 
        let expected = object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "field" .= ("val" :: Text)]
        let actual = toJSON (WithField (TestObj "key") (TestObj "val") 
              :: WithField "a" TestObj TestObj)
        expected @=? actual
    , TestLabel "Wrapper mode: atomic" $ TestCase $ do 
        let expected = object ["a" .= (0 :: Int), "value" .= ("val" :: Text)]
        let actual = toJSON (WithField 0 "val" :: WithField "a" Int String)
        expected @=? actual
    , TestLabel "Wrapper mode: array" $ TestCase $ do 
        let expected = object ["a" .= (0 :: Int), "value" .= (["val1", "val2"] :: [Text])]
        let actual = toJSON (WithField 0 ["val1", "val2"] :: WithField "a" Int [String])
        expected @=? actual
    , TestLabel "Wrapper mode: complex field" $ TestCase $ do 
        let expected = object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "value" .= ("val":: Text)]
        let actual = toJSON (WithField (TestObj "key") "val" :: WithField "a" TestObj String)
        expected @=? actual
    ]
  testsFromJSON = TestList [
      TestLabel "Inline mode: atomic field" $ TestCase $ do 
        let Success (expected :: WithField "a" Int TestObj) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "field" .= ("val" :: Text)]
        let actual = WithField 0 (TestObj "val") :: WithField "a" Int TestObj
        expected @=? actual
    , TestLabel "Inline mode: complex field" $ TestCase $ do 
        let Success (expected :: WithField "a" TestObj TestObj) = fromJSON $ object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "field" .= ("val" :: Text)]
        let actual = WithField (TestObj "key") (TestObj "val") :: WithField "a" TestObj TestObj
        expected @=? actual
    , TestLabel "Wrapper mode: atomic" $ TestCase $ do 
        let Success (expected :: WithField "a" Int String) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "value" .= ("val" :: Text)]
        let actual = WithField 0 "val" :: WithField "a" Int String
        expected @=? actual
    , TestLabel "Wrapper mode: array" $ TestCase $ do 
        let Success (expected :: WithField "a" Int [String]) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "value" .= (["val1", "val2"] :: [Text]) ]
        let actual = WithField 0 ["val1", "val2"] :: WithField "a" Int [String]
        expected @=? actual
    , TestLabel "Wrapper mode: complex field" $ TestCase $ do 
        let Success (expected :: WithField "a" TestObj String) = fromJSON $ object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "value" .= ("val":: Text)]
        let actual = WithField (TestObj "key") "val" :: WithField "a" TestObj String
        expected @=? actual
    ]
  testsToSchema = TestList [
      TestLabel "Inline mode: atomic field" $ TestCase $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("field", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int TestObj))
        expected @=? (actual ^. properties)
    , TestLabel "Inline mode: complex field" $ TestCase $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy TestObj))
              , ("field", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" TestObj TestObj))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: atomic" $ TestCase $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("value", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int String))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: array" $ TestCase $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("value", Inline $ toSchema (Proxy :: Proxy [String]))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int [String]))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: complex field" $ TestCase $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy TestObj))
              , ("value", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" TestObj String))
        expected @=? (actual ^. properties)
    ]

data TestObj1 = TestObj1 !Text 
  deriving (Eq, Show)

instance ToJSON TestObj1 where 
  toJSON (TestObj1 t) = object ["field1" .= t]
instance FromJSON TestObj1 where 
  parseJSON (Object o) = TestObj1 <$> o .: "field1"
  parseJSON _ = mzero
instance ToSchema TestObj1 where 
  declareNamedSchema prx = do 
    t <- declareSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "TestObj1") $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ [("field1", Inline t)]
      & required .~ ["field1"]

data TestObj2 = TestObj2 !Text 
  deriving (Eq, Show)

instance ToJSON TestObj2 where 
  toJSON (TestObj2 t) = object ["field2" .= t]
instance FromJSON TestObj2 where 
  parseJSON (Object o) = TestObj2 <$> o .: "field2"
  parseJSON _ = mzero
instance ToSchema TestObj2 where 
  declareNamedSchema prx = do 
    t <- declareSchema (Proxy :: Proxy Text)
    return $ NamedSchema (Just "TestObj2") $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ [("field2", Inline t)]
      & required .~ ["field2"]

withFieldsTests :: Test
withFieldsTests = TestList [
    TestLabel "toJSON" testsToJSON
  , TestLabel "fromJSON" testsFromJSON
  , TestLabel "toSchema" testsToSchema
  ]
  where
  testsToJSON = TestList [
      TestLabel "Inline mode" $ TestCase $ do 
        let expected = object [
                "field1" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields (TestObj1 "val1") (TestObj2 "val2"))
        expected @=? actual
    , TestLabel "Wrapper mode: first" $ TestCase $ do 
        let expected = object [
                "injected" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields ("val1" :: Text) (TestObj2 "val2"))
        expected @=? actual
    , TestLabel "Wrapper mode: second" $ TestCase $ do 
        let expected = object [
                "field1" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields (TestObj1 "val1") ("val2" :: Text))
        expected @=? actual
    , TestLabel "Wrapper mode: both" $ TestCase $ do 
        let expected = object [
                "injected" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields ("val1" :: Text) ("val2" :: Text))
        expected @=? actual
    ]
  testsFromJSON = TestList [
      TestLabel "Inline mode" $ TestCase $ do 
        let Success (expected :: WithFields TestObj1 TestObj2) = fromJSON $ object [
                "field1" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text)]
        let actual = WithFields (TestObj1 "val1") (TestObj2 "val2")
        expected @=? actual
    , TestLabel "Wrapper mode: first" $ TestCase $ do 
        let Success (expected :: WithFields Text TestObj2) = fromJSON $ object [
                "injected" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text)]
        let actual = WithFields ("val1" :: Text) (TestObj2 "val2")
        expected @=? actual
    , TestLabel "Wrapper mode: second" $ TestCase $ do 
        let Success (expected :: WithFields TestObj1 Text) = fromJSON $ object [
                "field1" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text)]
        let actual = WithFields (TestObj1 "val1") ("val2" :: Text)
        expected @=? actual
    , TestLabel "Wrapper mode: both" $ TestCase $ do 
        let Success (expected :: WithFields Text Text) = fromJSON $ object [
                "injected" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text)]
        let actual = WithFields ("val1" :: Text) ("val2" :: Text)
        expected @=? actual
    ]
  testsToSchema = TestList [
      TestLabel "Inline mode" $ TestCase $ do 
        let expected = [
                ("field1", Inline $ toSchema (Proxy :: Proxy Text))
              , ("field2", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields TestObj1 TestObj2))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: first" $ TestCase $ do 
        let expected = [
                ("injected", Inline $ toSchema (Proxy :: Proxy Text))
              , ("field2", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields Text TestObj2))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: second" $ TestCase $ do 
        let expected = [
                ("field1", Inline $ toSchema (Proxy :: Proxy Text))
              , ("value", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields TestObj1 Text))
        expected @=? (actual ^. properties)
    , TestLabel "Wrapper mode: both" $ TestCase $ do 
        let expected = [
                ("injected", Inline $ toSchema (Proxy :: Proxy Text))
              , ("value", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields Text Text))
        expected @=? (actual ^. properties)
    ]
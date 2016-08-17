{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
module Main where

import Control.Lens hiding ((.=))
import Control.Monad 
import Data.Aeson as A 
import Data.Aeson.Unit
import Data.Aeson.WithField
import Data.Monoid 
import Data.Proxy
import Data.Scientific (scientific)
import Data.Swagger
import Data.Swagger.Internal.Schema
import Data.Text
import Data.Text.Arbitrary
import Data.Typeable 
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import qualified Data.Vector as V 

instance (Arbitrary a, Arbitrary b) => Arbitrary (WithField s a b) where 
  arbitrary = WithField <$> arbitrary <*> arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (WithFields a b) where 
  arbitrary = WithFields <$> arbitrary <*> arbitrary

instance Arbitrary a => Arbitrary (OnlyField s a) where 
  arbitrary = OnlyField <$> arbitrary

instance Arbitrary Value where 
  arbitrary = oneof [obj, arr]
    where 
    json = oneof [obj, arr, str, num, bl, nullg]
    obj = object <$> listOf ((.=) <$> arbitrary <*> json) 
    arr = Array . V.fromList <$> listOf json
    str = String <$> arbitrary
    num = fmap Number $ scientific <$> arbitrary <*> arbitrary
    bl = Bool <$> arbitrary
    nullg = pure Null 

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [qcProperties, unitTests]

qcProperties :: TestTree 
qcProperties = testGroup "Properties" [
    withFieldProps
  , withFieldsProps
  , onlyFieldProps
  , unitDataProps
  ]

unitTests :: TestTree 
unitTests = testGroup "Unit tests" [
    withFieldTests
  , withFieldsTests
  , onlyFieldTests
  , unitDataTests
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
instance Arbitrary TestObj where 
  arbitrary = TestObj <$> arbitrary

withFieldTests :: TestTree
withFieldTests = testGroup "WithField tests" [
    testsToJSON
  , testsFromJSON
  , testsToSchema
  ]
  where
  testsToJSON = testGroup "toJSON" [
      testCase "Inline mode: atomic field" $ do 
        let expected = object ["a" .= (0 :: Int), "field" .= ("val" :: Text)]
        let actual = toJSON (WithField 0 (TestObj "val") :: WithField "a" Int TestObj)
        expected @=? actual
    , testCase "Inline mode: complex field" $ do 
        let expected = object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "field" .= ("val" :: Text)]
        let actual = toJSON (WithField (TestObj "key") (TestObj "val") 
              :: WithField "a" TestObj TestObj)
        expected @=? actual
    , testCase "Wrapper mode: atomic" $ do 
        let expected = object ["a" .= (0 :: Int), "value" .= ("val" :: Text)]
        let actual = toJSON (WithField 0 "val" :: WithField "a" Int String)
        expected @=? actual
    , testCase "Wrapper mode: array" $ do 
        let expected = object ["a" .= (0 :: Int), "value" .= (["val1", "val2"] :: [Text])]
        let actual = toJSON (WithField 0 ["val1", "val2"] :: WithField "a" Int [String])
        expected @=? actual
    , testCase "Wrapper mode: complex field" $ do 
        let expected = object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "value" .= ("val":: Text)]
        let actual = toJSON (WithField (TestObj "key") "val" :: WithField "a" TestObj String)
        expected @=? actual
    , testCase "Overwrite mode" $ do 
        let expected = object ["a" .= (0 :: Int)]
        let actual = toJSON (WithField 0 (OnlyField "val") :: WithField "a" Int (OnlyField "a" Text))
        expected @=? actual
    , testCase "Overwrite mode: wrapper" $ do 
        let expected = object ["value" .= (0 :: Int)]
        let actual = toJSON (WithField 0 "val" :: WithField "value" Int Text)
        expected @=? actual
    ]
  testsFromJSON = testGroup "fromJSON" [
      testCase "Inline mode: atomic field" $ do 
        let A.Success (expected :: WithField "a" Int TestObj) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "field" .= ("val" :: Text)]
        let actual = WithField 0 (TestObj "val") :: WithField "a" Int TestObj
        expected @=? actual
    , testCase "Inline mode: complex field" $ do 
        let A.Success (expected :: WithField "a" TestObj TestObj) = fromJSON $ object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "field" .= ("val" :: Text)]
        let actual = WithField (TestObj "key") (TestObj "val") :: WithField "a" TestObj TestObj
        expected @=? actual
    , testCase "Wrapper mode: atomic" $ do 
        let A.Success (expected :: WithField "a" Int String) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "value" .= ("val" :: Text)]
        let actual = WithField 0 "val" :: WithField "a" Int String
        expected @=? actual
    , testCase "Wrapper mode: array" $ do 
        let A.Success (expected :: WithField "a" Int [String]) = fromJSON $ object [
                "a" .= (0 :: Int)
              , "value" .= (["val1", "val2"] :: [Text]) ]
        let actual = WithField 0 ["val1", "val2"] :: WithField "a" Int [String]
        expected @=? actual
    , testCase "Wrapper mode: complex field" $ do 
        let A.Success (expected :: WithField "a" TestObj String) = fromJSON $ object [
                "a" .= object ["field" .= ("key" :: Text)]
              , "value" .= ("val":: Text)]
        let actual = WithField (TestObj "key") "val" :: WithField "a" TestObj String
        expected @=? actual
    ]
  testsToSchema = testGroup "toSchema" [
      testCase "Inline mode: atomic field" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("field", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int TestObj))
        expected @=? (actual ^. properties)
    , testCase "Inline mode: complex field" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy TestObj))
              , ("field", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" TestObj TestObj))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: atomic" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("value", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int String))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: array" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int))
              , ("value", Inline $ toSchema (Proxy :: Proxy [String]))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" Int [String]))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: complex field" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy TestObj))
              , ("value", Inline $ toSchema (Proxy :: Proxy String))]
        let actual = toSchema (Proxy :: Proxy (WithField "a" TestObj String))
        expected @=? (actual ^. properties)
    ]

data TestObj1 a = TestObj1 !a 
  deriving (Eq, Show)

instance ToJSON a => ToJSON (TestObj1 a) where 
  toJSON (TestObj1 t) = object ["field1" .= t]
instance FromJSON a => FromJSON (TestObj1 a) where 
  parseJSON (Object o) = TestObj1 <$> o .: "field1"
  parseJSON _ = mzero
instance (ToSchema a, Typeable a) => ToSchema (TestObj1 a) where 
  declareNamedSchema prx = do 
    t <- declareSchema (Proxy :: Proxy a)
    let nm = pack . show $ typeRep (Proxy :: Proxy a)
    return $ NamedSchema (Just $ "TestObj1'" <> nm) $ mempty 
      & type_ .~ SwaggerObject
      & properties .~ [("field1", Inline t)]
      & required .~ ["field1"]
instance Arbitrary a => Arbitrary (TestObj1 a) where 
  arbitrary = TestObj1 <$> arbitrary

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
instance Arbitrary TestObj2 where 
  arbitrary = TestObj2 <$> arbitrary

withFieldsTests :: TestTree
withFieldsTests = testGroup "WithFields tests" [
    testsToJSON
  , testsFromJSON
  , testsToSchema
  ]
  where
  testsToJSON = testGroup "toJSON" [
      testCase "Inline mode" $ do 
        let expected = object [
                "field1" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields (TestObj1 "val1" :: TestObj1 Text) (TestObj2 "val2"))
        expected @=? actual
    , testCase "Wrapper mode: first" $ do 
        let expected = object [
                "injected" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields ("val1" :: Text) (TestObj2 "val2"))
        expected @=? actual
    , testCase "Wrapper mode: second" $ do 
        let expected = object [
                "field1" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields (TestObj1 "val1" :: TestObj1 Text) ("val2" :: Text))
        expected @=? actual
    , testCase "Wrapper mode: both" $ do 
        let expected = object [
                "injected" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text) ]
        let actual = toJSON (WithFields ("val1" :: Text) ("val2" :: Text))
        expected @=? actual
    , testCase "Overwrite mode" $ do 
        let expected = object ["field1" .= ("val1" :: Text) ]
        let actual = toJSON (WithFields (TestObj1 "val1" :: TestObj1 Text) (TestObj1 "val2" :: TestObj1 Text))
        expected @=? actual
    , testCase "Overwrite mode: wrapper first" $ do 
        let expected = object ["injected" .= ("val1" :: Text) ]
        let actual = toJSON (WithFields ("val1" :: Text) (OnlyField "val2" :: OnlyField "injected" Text))
        expected @=? actual
    , testCase "Overwrite mode: wrapper second" $ do 
        let expected = object ["value" .= ("val1" :: Text) ]
        let actual = toJSON (WithFields (OnlyField "val1" :: OnlyField "value" Text) ("val2" :: Text))
        expected @=? actual
    ]
  testsFromJSON = testGroup "fromJSON" [
      testCase "Inline mode" $ do 
        let A.Success (expected :: WithFields (TestObj1 Text) TestObj2) = fromJSON $ object [
                "field1" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text)]
        let actual = WithFields (TestObj1 "val1") (TestObj2 "val2")
        expected @=? actual
    , testCase "Wrapper mode: first" $ do 
        let A.Success (expected :: WithFields Text TestObj2) = fromJSON $ object [
                "injected" .= ("val1" :: Text)
              , "field2" .= ("val2" :: Text)]
        let actual = WithFields ("val1" :: Text) (TestObj2 "val2")
        expected @=? actual
    , testCase "Wrapper mode: second" $ do 
        let A.Success (expected :: WithFields (TestObj1 Text) Text) = fromJSON $ object [
                "field1" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text)]
        let actual = WithFields (TestObj1 "val1") ("val2" :: Text)
        expected @=? actual
    , testCase "Wrapper mode: both" $ do 
        let A.Success (expected :: WithFields Text Text) = fromJSON $ object [
                "injected" .= ("val1" :: Text)
              , "value" .= ("val2" :: Text)]
        let actual = WithFields ("val1" :: Text) ("val2" :: Text)
        expected @=? actual
    , testCase "Overwrite mode" $ do 
        let A.Success (expected :: WithFields (TestObj1 Text) (TestObj1 Text)) = fromJSON $ object [
                "field1" .= ("val1" :: Text) ]
        let actual = WithFields (TestObj1 "val1") (TestObj1 "val1")
        expected @=? actual
    , testCase "Overwrite mode: wrapper first" $ do 
        let A.Success (expected :: WithFields Text (OnlyField "injected" Text)) = fromJSON $ object [
                "injected" .= ("val1" :: Text) ]
        let actual = WithFields "val1" (OnlyField "val1" :: OnlyField "injected" Text)
        expected @=? actual
    , testCase "Overwrite mode: wrapper second" $ do 
        let A.Success (expected :: WithFields (OnlyField "value" Text) Text) = fromJSON $ object [
                "value" .= ("val1" :: Text) ]
        let actual = WithFields (OnlyField "val1" :: OnlyField "value" Text) "val1" 
        expected @=? actual
    ]
  testsToSchema = testGroup "toSchema" [
      testCase "Inline mode" $ do 
        let expected = [
                ("field1", Inline $ toSchema (Proxy :: Proxy Text))
              , ("field2", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields (TestObj1 Text) TestObj2))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: first" $ do 
        let expected = [
                ("injected", Inline $ toSchema (Proxy :: Proxy Text))
              , ("field2", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields Text TestObj2))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: second" $ do 
        let expected = [
                ("field1", Inline $ toSchema (Proxy :: Proxy Text))
              , ("value", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields (TestObj1 Text) Text))
        expected @=? (actual ^. properties)
    , testCase "Wrapper mode: both" $ do 
        let expected = [
                ("injected", Inline $ toSchema (Proxy :: Proxy Text))
              , ("value", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields Text Text))
        expected @=? (actual ^. properties)
    , testCase "Overwrite mode" $ do 
        let expected = [("field1", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields (TestObj1 Text) (TestObj1 Int)))
        expected @=? (actual ^. properties)
    , testCase "Overwrite mode: wrapper first" $ do 
        let expected = [("injected", Inline $ toSchema (Proxy :: Proxy Text))]
        let actual = toSchema (Proxy :: Proxy (WithFields Text (OnlyField "injected" Int)))
        expected @=? (actual ^. properties)
    , testCase "Overwrite mode: wrapper second" $ do 
        let expected = [("value", Inline $ toSchema (Proxy :: Proxy Int))]
        let actual = toSchema (Proxy :: Proxy (WithFields (OnlyField "value" Int) Text))
        expected @=? (actual ^. properties)
    ]

onlyFieldTests :: TestTree
onlyFieldTests = testGroup "OnlyField tests" [
    testsToJSON
  , testsFromJSON
  , testsToSchema
  ]
  where 
  testsToJSON = testGroup "toJSON" [
      testCase "Normal mode" $ do 
        let expected = object [ "a" .= (42 :: Int) ]
        let actual = toJSON (OnlyField 42 :: OnlyField "a" Int)
        expected @=? actual
    ]
  testsFromJSON = testGroup "FromJSON" [
      testCase "Normal mode" $ do 
        let expected = OnlyField 42 :: OnlyField "a" Int
        let (A.Success (actual :: OnlyField "a" Int)) = fromJSON $ object [ 
              "a" .= (42 :: Int) ]
        expected @=? actual
    ]
  testsToSchema = testGroup "ToSchema" [
      testCase "Normal mode" $ do 
        let expected = [
                ("a", Inline $ toSchema (Proxy :: Proxy Int)) ]
        let actual = toSchema (Proxy :: Proxy (OnlyField "a" Int))
        expected @=? (actual ^. properties)
    ]

unitDataTests :: TestTree
unitDataTests = testGroup "Unit tests" [
    testsToJSON 
  , testsFromJSON 
  , testsToSchema
  ]
  where 
  testsToJSON = testGroup "toJSON" [
      testCase "Normal mode" $ do 
        let expected = object [ ]
        let actual = toJSON Unit
        expected @=? actual
    ]
  testsFromJSON = testGroup "FromJSON" [
      testCase "Normal mode" $ do 
        let expected = Unit
        let (A.Success (actual :: Unit)) = fromJSON $ object [ ]
        expected @=? actual
    ]
  testsToSchema = testGroup "ToSchema" [
      testCase "Normal mode" $ do 
        let expected = NamedSchema Nothing $ mempty & type_ .~ SwaggerObject
        let actual = toNamedSchema (Proxy :: Proxy Unit)
        expected @=? actual
    ]

withFieldProps :: TestTree
withFieldProps = testGroup "withField properties" [
    functorProps
  , bifunctorProps
  ]
  where 
    functorProps = QC.testProperty "fmap id  ==  id" $  \(wf :: WithField "id" Int TestObj) -> 
      fmap id wf == wf 
    bifunctorProps = QC.testProperty "bimap id id == id" $  \(wf :: WithField "id" Int TestObj) -> 
      bimap id id wf == wf 

withFieldsProps :: TestTree
withFieldsProps = testGroup "withFields properties" [
    functorProps
  , bifunctorProps
  ]
  where 
    functorProps = QC.testProperty "fmap id  ==  id" $  \(wf :: WithFields (TestObj1 Text) TestObj2) -> 
      fmap id wf == wf 
    bifunctorProps = QC.testProperty "bimap id id == id" $  \(wf :: WithFields (TestObj1 Text) TestObj2) -> 
      bimap id id wf == wf 

onlyFieldProps :: TestTree 
onlyFieldProps = testGroup "onlyField properties" [
    functorProps
  ]
  where 
    functorProps = QC.testProperty "fmap id  ==  id" $  \(wf :: OnlyField "id" TestObj) -> 
      fmap id wf == wf

unitDataProps :: TestTree 
unitDataProps = testGroup "Unit properties" [
    parseProps
  ]
  where 
    parseProps = QC.testProperty "parseJSON Unit not fails" $ \(json :: Value) -> 
      case fromJSON json of 
        A.Success Unit -> True 
        _ -> False 

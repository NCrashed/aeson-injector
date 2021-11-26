# aeson-injector

[![Build Status](https://travis-ci.org/NCrashed/aeson-injector.svg?branch=master)](https://travis-ci.org/NCrashed/aeson-injector)

It is small utility library that is intented to be used in RESTful APIs, especially with [servant](http://haskell-servant.readthedocs.io/en/stable/) and [Swagger](http://swagger.io/). Its main purpose is simple injection of fields into JSONs produced by [aeson](https://hackage.haskell.org/package/aeson) library.

Consider the following common data type in web service developing:

``` haskell
data News = News {
  title :: Text
, body :: Text
, author :: Text
, timestamp :: UTCTime  
}

-- Consider we have simple 'ToJSON' and 'FromJSON' instances
$(deriveJSON defaultOptions ''News) 
```

[ToJSON](http://hackage.haskell.org/package/aeson-0.11.2.0/docs/Data-Aeson.html#t:ToJSON) instance produces JSON's like:

``` json
{
  "title": "Awesome piece of news!"
, "body": "Big chunk of text"
, "author": "Just Me"
, "timestamp": "2016-07-26T18:54:42.678999Z"
}
```

Now one can create a simple web server with servant DSL:

``` haskell
type NewsId = Word 

type NewsAPI = 
       ReqBody '[JSON] News :> Post '[JSON] NewsId
  :<|> Capture "news-id" NewsId :> Get '[JSON] News
  :<|> "list" :> Get '[JSON] [News]
```

All seems legit, but, wait a second, an API user definitely would like to know id of news in `list` method. One way to do this is declare new type `NewsInfo` with additional field, but it is bad solution as requires to keep extra data type for each resource. 

So, here `aeson-injector` steps in, now you can write:
``` haskell
type NewsAPI = 
       ReqBody '[JSON] News :> Post '[JSON] NewsId
  :<|> Capture "news-id" NewsId :> Get '[JSON] News
  :<|> "list" :> Get '[JSON] [WithField "id" NewsId News]
``` 

`WithField "id" NewsId News` or simply `WithId NewsId News` wraps you data type and injects `id` field in produced JSON values:

``` haskell
>>> encode (WithField 42 myNews :: WithField "id" NewsId News)
```

``` json
{
  "id": 42
, "title": "Awesome piece of news!"
, "body": "Big chunk of text"
, "author": "Just Me"
, "timestamp": "2016-07-26T18:54:42.678999Z"
}
```

`WithField` data type has `FromJSON` instance for seamless parsing of data with injected fields and [ToSchema](https://hackage.haskell.org/package/swagger2-2.1/docs/Data-Swagger-Internal-Schema.html#t:ToSchema) instance for [servant-swagger](https://hackage.haskell.org/package/servant-swagger) support.

## Injecting multiple values

The library also has more general data type `WithFields a b` that injects fields of 'toJSON a' into `toJSON b`. 

``` haskell
data NewsPatch = NewsPatch {
  tags :: [Text]
, rating :: Double
}
$(deriveJSON defaultOptions ''NewsPatch) 
```

``` haskell
let myNewsPatch = NewsPatch ["tag1", "tag2"] 42 
in encode $ WithFields myNewsPatch myNews
```

``` json
{
  "title": "Awesome piece of news!"
, "body": "Big chunk of text"
, "author": "Just Me"
, "timestamp": "2016-07-26T18:54:42.678999Z"
, "tags": ["tag1", "tag2"]
, "rating": 42.0
}
```

## Corner cases

Unfortunately, we cannot inject in non object values of produced JSON, so the library creates a wrapper object around non-object value:

``` haskell
encode (WithId 0 "non-object" :: WithId Int String)
```

``` json
{
  "id": 0 
, "value": "non-object"
}
```

The same story is about `WithFields` data type:

``` haskell
encode (WithFields 0 "non-object" :: WithFields Int String)
```

``` json
{
  "injected": 0 
, "value": "non-object"
}
```

For more examples and details, please, follow the haddocks.

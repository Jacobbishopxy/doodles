{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- file: SwaggerApi.hs
-- author: Jacob Xie
-- date: 2024/07/15 13:29:22 Monday
-- brief: https://github.com/haskell-servant/servant-swagger/blob/master/example/src/Todo.hs

module SwaggerApi where

import Control.Lens
import Data.Aeson (FromJSON, ToJSON, toJSON)
import Data.Swagger
import Data.Text (Text)
import Data.Time (UTCTime (UTCTime), fromGregorian)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant
import Servant.Swagger

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

type TodoAPI =
  "todo" :> Get '[JSON] [Todo]
    :<|> "todo" :> ReqBody '[JSON] Todo :> Post '[JSON] TodoId
    :<|> "todo" :> Capture "id" TodoId :> Get '[JSON] Todo
    :<|> "todo" :> Capture "id" TodoId :> ReqBody '[JSON] Todo :> Put '[JSON] TodoId

type SwapperAPI =
  "swagger.json" :> Get '[JSON] Swagger

type API = SwapperAPI :<|> TodoAPI

data Todo = Todo
  { created :: UTCTime,
    summary :: Text
  }
  deriving (Show, Generic, Typeable)

newtype TodoId = TodoId Int
  deriving (Show, Generic, Typeable, ToJSON, FromHttpApiData)

instance ToJSON Todo

instance FromJSON Todo

instance ToSchema Todo where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is some real Todo right here"
      & mapped . schema . example ?~ toJSON (Todo (UTCTime (fromGregorian 2024 7 15) 0) "get milk")

instance ToParamSchema TodoId

instance ToSchema TodoId

todoSwagger :: Swagger
todoSwagger =
  toSwagger todoAPI
    & info . title .~ "Todo API"
    & info . version .~ "0.0"
    & info . description ?~ "This is an API that tests swagger integration"
    & info . license ?~ ("MIT" & url ?~ URL "http://mit.com")

swaggerServer :: Server API
swaggerServer = return todoSwagger :<|> error "not implemented"

api :: Proxy API
api = Proxy

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
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
import Data.Text (Text, unpack)
import Data.Time (UTCTime (UTCTime), fromGregorian, getCurrentTime)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Servant
import Servant.Swagger
import Servant.Swagger.UI

todoAPI :: Proxy TodoAPI
todoAPI = Proxy

type TodoAPI =
  "time" :> Summary "time" :> Get '[PlainText] String
    :<|> "todo"
      :> Summary "retrieve all todos"
      :> Get '[JSON] [Todo]
    :<|> "todo"
      :> Summary "get a todo"
      :> Capture "id" TodoId
      :> Get '[JSON] Todo
    :<|> "todo"
      :> Summary "save a todo"
      :> ReqBody '[JSON] Todo
      :> Post '[JSON] TodoId
    :<|> "todo"
      :> Summary "update a todo by :id"
      :> Capture "id" TodoId
      :> ReqBody '[JSON] Todo
      :> Put '[JSON] Todo
    :<|> "todo"
      :> Summary "delete a todo by :id"
      :> Capture "id" TodoId
      :> Delete '[JSON] Todo

type API = SwaggerSchemaUI "swagger-ui" "swagger.json" :<|> TodoAPI

data Todo = Todo
  { brief :: Text,
    contents :: Text,
    priority :: Int,
    created :: Maybe UTCTime,
    modified :: Maybe UTCTime
  }
  deriving (Show, Generic, Typeable, ToJSON, FromJSON)

instance ToSchema Todo where
  declareNamedSchema proxy =
    genericDeclareNamedSchema defaultSchemaOptions proxy
      & mapped . schema . description ?~ "This is some real Todo right here"
      & mapped . schema . example ?~ toJSON mockTodo

newtype TodoId = TodoId Int
  deriving (Show, Eq, Ord, Generic, Typeable, ToJSON, ToParamSchema, ToSchema)

instance FromHttpApiData TodoId where
  parseUrlPiece txt =
    case reads (unpack txt) of
      [(i, "")] -> Right (TodoId i)
      _ -> Left "Failed to parse TodoId"

newTodo :: Text -> Text -> Int -> IO Todo
newTodo b c p =
  getCurrentTime >>= \tm ->
    return $ Todo b c p (Just tm) (Just tm)

mkNewTodo :: Todo -> IO Todo
mkNewTodo t =
  getCurrentTime >>= \tm ->
    return $ t {created = Just tm, modified = Just tm}

mockTodo :: Todo
mockTodo =
  Todo "get milk" "goto grocery store and buy a bottle of milk" 2 t t
  where
    t = Just $ UTCTime (fromGregorian 2024 7 15) 0

modifyTodo :: Todo -> Todo -> IO Todo
modifyTodo newT todo = do
  getCurrentTime >>= \t ->
    return $
      todo
        { brief = brief newT,
          contents = contents newT,
          priority = priority newT,
          modified = Just t
        }

----------------------------------------------------------------------------------------------------

swaggerDoc :: Swagger
swaggerDoc =
  toSwagger todoAPI
    & info . title .~ "Todo API"
    & info . version .~ "0.0.1"
    & info . description ?~ "This is an API that tests swagger integration"
    & info . license ?~ ("BSD 3.0" & url ?~ URL "https://opensource.org/licenses/BSD-3-Clause")

api :: Proxy API
api = Proxy

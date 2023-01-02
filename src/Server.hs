{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import Control.Applicative (empty)
import Core.Todo
import Data.Aeson
import qualified Data.ByteString.Internal as B (ByteString)
import qualified Data.ByteString.Lazy.Internal as LB (ByteString)
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

-- HTTP Header
jsonContentType :: (HeaderName, B.ByteString)
jsonContentType = (hContentType, "application/json")

-- Entity
data CreateRequest = CreateRequest
  { getDescriptionReq :: Text,
    getPriorityReq :: Int
  }

-- JSON deserialization
instance FromJSON CreateRequest where
  parseJSON (Object v) = CreateRequest <$> v .: "getDescriptionReq" <*> v .: "getPriorityReq"
  parseJSON _ = empty

-- Server
serve :: IO ()
serve = do
  let port = 3000
  putStrLn $ "Listening on port " ++ show port
  run port app

-- Router
app :: Application
app request send = route request >>= send
  where
    route req = case rawPathInfo req of
      "/" -> listRoute
      "/create" -> createRoute req
      _ -> notFoundRoute

-- Routes
listRoute :: IO Response
listRoute = listTodo >>= respond status200 . encode

createRoute :: Request -> IO Response
createRoute request = doDecode request >>= doCreate >>= doRespond
  where
    doDecode req = decode <$> lazyRequestBody req
    doCreate (Just req) = createTodo (getDescriptionReq req) (getPriorityReq req) >> return (Right "201 - Created")
    doCreate Nothing = return $ Left "400 - Bad request"
    doRespond = either (respond status201) (respond status400)

notFoundRoute :: IO Response
notFoundRoute = respond status404 "404 - Not Found"

-- Helpers
respond :: Status -> LB.ByteString -> IO Response
respond status resp = return $ responseLBS status [jsonContentType] resp

{-# LANGUAGE OverloadedStrings #-}

module Server (serve) where

import Control.Applicative (empty)
import Data.Aeson
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Internal as B (ByteString)
import Data.Text (Text)
import Network.HTTP.Types
import Network.Wai
import Network.Wai.Handler.Warp

import Core.Todo (listTodo, createTodo)

jsonContentType :: (HeaderName, B.ByteString)
jsonContentType = (hContentType, "application/json")

data CreateRequest = CreateRequest {
    getDescription :: Text,
    getPriority :: Int
}

instance FromJSON CreateRequest where
    parseJSON (Object v) = CreateRequest <$> v .: "description"
                                         <*> v .: "priority"
    parseJSON _ = empty

serve :: IO ()
serve = do
    let port = 3000
    putStrLn $ "Listening on port " ++ show port
    run port app

app :: Application
app req send = case rawPathInfo req of
        "/" -> listRoute
        "/create" -> createRoute req
        _ -> notFoundRoute
    >>= send

listRoute :: IO Response
listRoute = responseLBS status200 [jsonContentType] . encode <$> listTodo

createRoute :: Request -> IO Response
createRoute req = doRespond <$> (doDecode req >>= doCreate)

notFoundRoute :: IO Response
notFoundRoute = return $ responseLBS status404 [jsonContentType] "404 - Not Found"

doDecode :: Request -> IO (Maybe CreateRequest)
doDecode req = decode <$> lazyRequestBody req

doCreate :: Maybe CreateRequest -> IO (Either ByteString ByteString)
doCreate (Just req) = createTodo (getDescription req) (getPriority req) >> return (Right "201 - Created")
doCreate Nothing = return $ Left "400 - Bad request" 

doRespond :: Either ByteString ByteString -> Response
doRespond (Right msg) = responseLBS status201 [jsonContentType] msg
doRespond (Left msg) = responseLBS status400 [jsonContentType] msg

{-# LANGUAGE OverloadedStrings #-}

module Core.Todo (Todo (..), listTodo, createTodo) where

import Control.Applicative (empty)
import Data.Aeson (FromJSON(parseJSON), Value(Object), ToJSON(toJSON), (.:), object, KeyValue((.=)))
import Data.Text (Text)
import Database.SQLite.Simple (FromRow, fromRow, field, open, close, execute, query_)

data Todo = Todo {
    id :: Int,
    description :: Text,
    priority :: Int
} deriving Show

instance ToJSON Todo where
    toJSON (Todo id description priority) = object [
        "id" .= id,
        "description" .= description,
        "priority" .= priority
        ]

instance FromJSON Todo where
    parseJSON (Object v) = Todo <$> v .: "id"
                                <*> v .: "description"
                                <*> v .: "priority"
    parseJSON _ = empty

instance FromRow Todo where
    fromRow = Todo <$> field
                   <*> field
                   <*> field

listTodo :: IO [Todo]
listTodo = do
    conn <- open "todo.db"
    resp <- query_ conn "SELECT * FROM todo"
    close conn
    return resp

createTodo :: Text -> Int -> IO ()
createTodo description priority = do
    conn <- open "todo.db"
    execute conn "INSERT INTO todo (description, priority) VALUES (?,?)" (description, priority)
    close conn

{-# LANGUAGE OverloadedStrings #-}

module Core.Todo (Todo (..), listTodo, createTodo) where

import Control.Applicative (empty)
import Data.Aeson
import Data.Text (Text)
import Database.SQLite.Simple

-- Entity
data Todo = Todo
  { getId :: Int,
    getDescription :: Text,
    getPriority :: Int
  }
  deriving (Show)

-- JSON (de)serialization
instance ToJSON Todo where
  toJSON (Todo uid description priority) =
    object
      [ "getId" .= uid,
        "getDescription" .= description,
        "getPriority" .= priority
      ]

instance FromJSON Todo where
  parseJSON (Object v) =
    Todo <$> v .: "getId"
      <*> v .: "getDescription"
      <*> v .: "getPriority"
  parseJSON _ = empty

-- SQLite mapping
instance FromRow Todo where
  fromRow = Todo <$> field <*> field <*> field

-- Database operations
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

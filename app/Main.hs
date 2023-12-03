{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ViewPatterns #-}

import Yesod
import Yesod.Core
import Network.HTTP.Types.Status
import Data.Aeson (genericParseJSON, genericToJSON, defaultOptions)
import GHC.Generics (Generic)
import Database.Persist.Sqlite
import Data.Text (Text)

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
  title String
  desc  String
  done  Bool
  from  String
  till  String
  deriving Show Generic
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/                   TodosR  GET POST
/#TodoId TodoR   DELETE
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

instance ToJSON Todo where
  toJSON = genericToJSON defaultOptions

instance FromJSON Todo where
  parseJSON = genericParseJSON defaultOptions

getTodosR :: Handler Value
getTodosR = do
  todos <- runDB $ selectList [] [Asc TodoTitle]
  returnJson $ map entityVal todos

postTodosR :: Handler String
postTodosR = do
  newTodo <- requireCheckJsonBody :: Handler Todo
  todoId <- runDB $ insert newTodo
  return $ show todoId

deleteTodoR :: TodoId -> Handler String
deleteTodoR todoId = do
  maybeTodo <- runDB $ get todoId
  case maybeTodo of
    Nothing -> do
      sendResponseStatus status404 ("Todo item not found" :: Text)
    Just _ -> do
      runDB $ delete todoId
      sendResponseStatus status204 ("Todo item deleted" :: Text)
  

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "people.db3" 10 $ \pool -> liftIO $ do
  runResourceT $ flip runSqlPool pool $ do
    runMigration migrateAll
  warp 3000 $ App pool

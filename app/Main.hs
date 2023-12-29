{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DeriveAnyClass             #-}
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
import GHC.Generics (Generic)
import Database.Persist.Sqlite
import Data.Text (Text)
import Data.Aeson (Result(Success, Error))

import Control.Monad.Trans.Resource (runResourceT)
import Control.Monad.Logger (runStderrLoggingT)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Todo
  title String
  desc  String
  done  Bool
  from  String
  till  String
  deriving Show Generic ToJSON FromJSON
|]

data App = App ConnectionPool

mkYesod "App" [parseRoutes|
/        TodosR  GET POST
/#TodoId TodoR   DELETE PATCH
|]

instance Yesod App

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    App pool <- getYesod
    runSqlPool action pool

getTodosR :: Handler Value
getTodosR = do
  todos <- runDB $ selectList [] [Asc TodoId]
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
      sendResponseStatus status404 ("Todo item not found"::Text)
    Just _ -> do
      runDB $ delete todoId
      sendResponseStatus status204 ("Todo item deleted"::Text)

data UpdateRequest
  = ToggleDone
  | ChangeTitle String
  | ChangeDesc String
  | ChangeTill String

instance FromJSON UpdateRequest where
  parseJSON (Object v) = do
    (op::String)    <- v .: "op"
    (field::String) <- v .: "field"
    case op of
      "ChangeVal" -> do
        val <- v .: "value"
        case field of
          "title" -> return $ ChangeTitle val
          "desc"  -> return $ ChangeDesc val
          "till"  -> return $ ChangeTill val
      "ToggleDone" -> return ToggleDone

patchTodoR :: TodoId -> Handler String
patchTodoR todoId = do
  body <- parseCheckJsonBody :: Handler (Result UpdateRequest)
  maybeTodo <- runDB $ get todoId
  case maybeTodo of
    Nothing -> do
      sendResponseStatus status404 ("Todo item not found"::Text)
    Just todo -> do
      dbAction <- case body of
                    Success updateReq -> case updateReq of
                      ToggleDone ->
                        updatedb [TodoDone =. (not $ todoDone todo)]
                      ChangeTitle newTitle -> updatedb [TodoTitle =. newTitle]
                      ChangeDesc newDesc -> updatedb [TodoDesc =. newDesc]
                      ChangeTill newTill -> updatedb [TodoTill =. newTill]
                    Error _ -> sendResponseStatus status400 ("Invalid request"::Text)
      sendResponseStatus status204 ("Updated records"::Text)
  where
    updatedb dbAction = runDB $ update todoId dbAction

main :: IO ()
main = runStderrLoggingT $ withSqlitePool "people.db3" 10 $ \pool -> liftIO $ do
  runResourceT $ flip runSqlPool pool $ do
    runMigration migrateAll
  warp 3000 $ App pool

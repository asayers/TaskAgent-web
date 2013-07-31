{-# LANGUAGE OverloadedStrings #-}

-- | The API is vaguely RESTful. Here's an overview:
-- 
-- [@ GET \/api\/                  @] get names of lists (TODO: consider changing this endpoint)
--
-- [@ GET \/api\/:list             @] get all items in :list
--
-- [@ POST \/api\/:list {item}     @] add {item} to :list
--
-- [@ PUT \/api\/:list\/:id {item} @] replace item :id with {item} in :list
--
-- [@ DELETE \/api\/:list\/:id     @] remove item :id from :list
-- 
-- See the Todo module for implementation details.
module Main where

import Todo
import Auth
import Web.Scotty
import Control.Monad (join)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Web.ClientSession (getDefaultKey, Key)
import Network.HTTP.Types.Status (unauthorized401, ok200)
import Data.Aeson (object, (.=))
import Control.Error (fromMaybe)

hostUrl :: ByteString
hostUrl = "http://localhost:3001"

-- | Returns the email address of the logged in user
authenticate :: Key -> ActionM (Maybe String)
authenticate key = do
  authToken <- parseAuthToken <$> reqHeader "Cookie"
  let email = checkAuthToken key <$> authToken
  return $ join email

withAuthentication :: Key -> (String -> ActionM ()) -> ActionM ()
withAuthentication key fn = do
  email <- authenticate key
  case email of
    Nothing -> status unauthorized401
    Just e  -> fn e

-- | Print request parameters and body to stdout.
debug :: Key -> ActionM ()
debug key = do
  email <- authenticate key
  let authMsg = fromMaybe "Invalid" email
  ps <- show <$> params
  js <- show <$> body
  liftIO . putStrLn . unlines $ ["Auth: " ++ authMsg, "Params: " ++ ps, "Request Body: " ++ js]

-- TODO: catch exceptions thrown by Todo's exports and return informative error messages
main :: IO ()
main = scotty 3001 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "assets")
  key <- liftIO getDefaultKey
  get "/" $ redirect "/list/inbox"
  get "/list/:list" $ file "assets/index.html"
  get "/api/" $ withAuthentication key $ \email -> do
    lists <- liftIO (showLists email)
    json lists
  get "/api/:list" $ withAuthentication key $ \email -> do
    listName <- param "list"
    list <- liftIO $ loadList email listName
    json list
  post "/api/:list" $ withAuthentication key $ \email -> do
    list <- param "list"
    item <- jsonData
    liftIO $ addItem email list item
    text "success!"
  put "/api/:list/:id" $ withAuthentication key $ \email -> do
    list <- param "list"
    itemId <- param "id"
    item <- jsonData
    liftIO $ editItem email list itemId item
    text "success!"
  delete "/api/:list/:id" $ withAuthentication key $ \email -> do
    list <- param "list"
    itemId <- param "id"
    liftIO $ removeItem email list itemId
    text "success!"
  post "/auth/login" $ do
    assertion <- jsonData
    VerifierResponse authStatus email <- liftIO $ checkAssertion hostUrl assertion
    if authStatus == "okay"
      then do
        session <- liftIO $ encryptAndSerialise key email
        -- I would use a "Set-Cookie" header, but the request is handled by Angular so it doesn't work.
        json $ object ["session" .= session, "email" .= email]
      else status unauthorized401
  post "/auth/logout" $ status ok200
  notFound $ file "assets/404.html"

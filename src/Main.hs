{-# LANGUAGE OverloadedStrings #-}

-- | The API is vaguely RESTful. Here's an overview:
-- 
-- [@ GET \/api\/lists                  @] get names of lists (TODO: consider changing this endpoint)
--
-- [@ GET \/api\/list/:list             @] get all items in :list
--
-- [@ POST \/api\/list/:list {item}     @] add {item} to :list
--
-- [@ PUT \/api\/list/:list\/:id {item} @] replace item :id with {item} in :list
--
-- [@ DELETE \/api\/list/:list\/:id     @] remove item :id from :list
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
import Data.ByteString.Char8 (pack)
import Web.ClientSession (getDefaultKey, Key)
import Network.HTTP.Types.Status (unauthorized401, ok200)
import Data.Aeson (object, (.=))
import Network.Wai (requestHeaders)

-- | This is sent to the Persona verifier
hostUrl :: IO ByteString
hostUrl = pack <$> readFile "hostname.conf"

-- TODO: if the "Cookie" header isn't set, we just crash. Better to return nothing so we can send back a 401 status.
-- | Returns the email address of the logged in user
authenticate :: Key -> ActionM (Maybe String)
authenticate key = do
  cookie <- reqHeader "Cookie"
  let authToken = parseAuthToken cookie
      email     = checkAuthToken key <$> authToken
  return $ join email

-- | If a user is logged in, perform fn with their email address; otherwise, return an error.
withAuthentication :: Key -> (String -> ActionM ()) -> ActionM ()
withAuthentication key fn = do
  email <- authenticate key
  maybe (status unauthorized401) fn email

-- TODO: catch exceptions thrown by Todo's exports and return informative error messages
main :: IO ()
main = scotty 3001 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "assets")
  key <- liftIO getDefaultKey
  get "/" $ redirect "/list/inbox"
  get "/list/:list" $ file "assets/index.html"
  get "/api/lists" $ withAuthentication key $ \email -> do
      lists <- liftIO (showLists email)
      json lists
  get "/api/list/:list" $ withAuthentication key $ \email -> do
    listName <- param "list"
    list <- liftIO $ loadList email listName
    json list
  post "/api/list/:list" $ withAuthentication key $ \email -> do
    list <- param "list"
    item <- jsonData
    liftIO $ addItem email list item
    text "success!"
  put "/api/list/:list/:id" $ withAuthentication key $ \email -> do
    list <- param "list"
    itemId <- param "id"
    item <- jsonData
    liftIO $ editItem email list itemId item
    text "success!"
  delete "/api/list/:list/:id" $ withAuthentication key $ \email -> do
    list <- param "list"
    itemId <- param "id"
    liftIO $ removeItem email list itemId
    text "success!"
  post "/auth/login" $ do
    assertion <- jsonData
    host <- liftIO hostUrl
    VerifierResponse authStatus email <- liftIO $ checkAssertion host assertion
    if authStatus == "okay"
      then do
        session <- liftIO $ encryptAndSerialise key email
        -- I would use a "Set-Cookie" header, but Angular strips it.
        json $ object ["session" .= session, "email" .= email]
      else status unauthorized401
  post "/auth/logout" $ status ok200
  notFound $ file "assets/404.html"


-- | Print request parameters and body to stdout.
debug :: Key -> ActionM ()
debug key = do
  token <- parseAuthToken <$> reqHeader "Cookie"
  let email = join $ checkAuthToken key <$> token
  ps <- show <$> params
  js <- show <$> body
  headers <- show . requestHeaders <$> request
  liftIO . putStrLn . unlines $ ["AuthToken: " ++ show token, "User: " ++ show email,  "Params: " ++ ps, "Request Body: " ++ js, "Headers: " ++  headers]

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
import Web.Scotty (scotty, get, file, middleware, notFound, json, post, param, params, text, ActionM, jsonData, put, body, delete, status, reqHeader)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Web.ClientSession (getDefaultKey)
import Network.HTTP.Types.Status (unauthorized401, accepted202, ok200)
import Data.Aeson (object, (.=))

hostUrl :: ByteString
hostUrl = "http://localhost:3001"

-- | Print request parameters and body to stdout.
debug :: ActionM ()
debug = do
  ps <- show <$> params
  cs <- show <$> reqHeader "Cookie"
  js <- show <$> body
  liftIO . putStrLn . unlines $ ["Params: " ++ ps, "Cookies: " ++ cs, "JSON: " ++ js]

-- TODO: catch exceptions thrown by Todo's exports and return informative error messages
main :: IO ()
main = scotty 3001 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "assets")
  key <- liftIO getDefaultKey
  get "/test" $ debug >> text "test!"
  get "/api/" $ do
    lists <- liftIO $ showLists "asayers"
    json lists
  get "/api/:list" $ do
    list <- param "list"
    response <- liftIO $ loadList "asayers" list
    json response
  post "/api/:list" $ do
    list <- param "list"
    item <- jsonData
    liftIO $ addItem "asayers" list item
    text "success!"
  put "/api/:list/:id" $ do
    list <- param "list"
    itemId <- param "id"
    item <- jsonData
    liftIO $ editItem "asayers" list itemId item
    text "success!"
  delete "/api/:list/:id" $ do
    list <- param "list"
    itemId <- param "id"
    liftIO $ removeItem "asayers" list itemId
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
  post "/auth/logout" $ status accepted202
  notFound $ do
    status ok200
    file "assets/index.html"

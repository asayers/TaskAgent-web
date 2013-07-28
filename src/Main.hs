{-# LANGUAGE OverloadedStrings #-}

-- | GET    /api/                 - get names of lists (consider changind endpoint)
-- | GET    /api/:list            - get all items in :list
-- | POST   /api/:list {item}     - add {item} to :list
-- | PUT    /api/:list/:id {item} - replace item :id with {item} in :list
-- | DELETE /api/:list/:id        - remove item :id from :list
module Main where

import Todo
import Web.Scotty (scotty, get, file, middleware, notFound, json, post, param, params, text, ActionM, jsonData, put, body, delete)
import Control.Monad.IO.Class (liftIO)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static (staticPolicy, noDots, addBase, (>->))
import Control.Applicative ((<$>))

debug :: ActionM ()
debug = do
  ps <- show <$> params
  js <- show <$> body
  liftIO . putStrLn $ "Params: " ++ ps ++ "\nJSON: " ++ js

main :: IO ()
main = scotty 3000 $ do
  middleware logStdoutDev
  middleware $ staticPolicy (noDots >-> addBase "assets")
  get "/" $ file "assets/index.html"
  get "/api/" $ do
    lists <- liftIO showLists
    json lists
  get "/api/:list" $ do
    list <- param "list"
    response <- liftIO $ loadList list
    json response
  post "/api/:list" $ do
    list <- param "list"
    item <- jsonData
    liftIO $ addItem list item
    text "success!"
  put "/api/:list/:id" $ do
    list <- param "list"
    itemId <- param "id"
    item <- jsonData
    liftIO $ editItem list itemId item
    text "success!"
  delete "/api/:list/:id" $ do
    list <- param "list"
    itemId <- param "id"
    liftIO $ removeItem list itemId
    text "success!"
  notFound $ file "assets/index.html"
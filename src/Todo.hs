{-# LANGUAGE OverloadedStrings #-}

-- | This module deals with to-do lists and their various serialisations. There
-- | are three representations we use:
-- | - a simple String, for saving lists to disk;
-- | - an internal representation called List;
-- | - a JSON representation for passing to the front-end
-- |
-- | Disk Representation
-- | -------------------
-- | Each line represents a to-do, and begins either with '-' to indicate an
-- incomplete item, or 'x' to indicate a completed one. Note that the list is
-- ordered.
-- | Eg:
-- |     - One
-- |     x Two
-- |     - Three
-- |
-- | Internal Representation
-- | -----------------------
-- | A `List` is a list of `Item`s. Each item is either a `Complete itemBody` or an `Incomplete itemBody`.
-- | Eg:
-- |     [Complete "One", Incomplete "Two", Complete "Three"]
-- | 
-- | JSON Representation
-- | -------------------
-- | Items in the JSON representation are explicity numbered. This corresponds
-- to the numbering implicit in the ordering of the other representations. This
-- is so a single item may be modified by the front-end without having to
-- resend the entire list.
-- | Eg:
-- |     [
-- |       {body: "one", done: false},
-- |       {body: "two", done: true},
-- |       {body: "three", done: false}
-- |     ]
module Todo ( loadList
            , addItem
            , editItem
            , removeItem
            , showLists
            , Item (..)
            , List (..)
            ) where

import Control.Exception (bracketOnError)
import System.IO (openTempFile, hClose, hPutStr)
import System.Directory (removeFile, renameFile, doesFileExist, createDirectoryIfMissing, getDirectoryContents)
import Control.Monad (unless)
import System.FilePath ((</>), takeDirectory)
import Data.Aeson (ToJSON, FromJSON, toJSON, parseJSON, object, (.=), (.:), Value (..))
import Data.Aeson.Types (parseMaybe)
import Data.Text (pack)
import Control.Applicative ((<$>))

------------- Config -----------

listDirectory :: FilePath
listDirectory = "lists"

------------ Exports -----------

-- TODO: unhandled fileIO exceptions
loadList :: String -> IO List
loadList name = do
  file <- readFile $ listDirectory </> name
  case parseItems file of
    Nothing   -> error "Couldn't parse list"
    Just items -> return $ List items

addItem :: String -> Item -> IO ()
addItem listName item = do
  createListIfMissing listName
  appendFile (listDirectory </> listName) $ show item ++ "\n"

-- TODO: unhandled OOB exceptions
editItem :: String -> Int -> Item -> IO ()
editItem listName itemId item = do
  (List items) <- loadList listName
  let (xs, _:ys) = splitAt itemId items
  writeFile' (listDirectory </> listName) . show $ List (xs ++ [item] ++ ys)

-- TODO: unhandled OOB exceptions
removeItem :: String -> Int -> IO ()
removeItem listName itemId = do
  (List items) <- loadList listName
  let (xs, _:ys) = splitAt itemId items
  writeFile' (listDirectory </> listName) . show $ List (xs ++ ys)

showLists :: IO [String]
showLists = filter (notElem '.') <$> getDirectoryContents listDirectory

------- Type definitions -------

data List = List [Item] deriving (Eq)
data Item = Complete   { itemBody :: String }
          | Incomplete { itemBody :: String }
          deriving (Eq)

instance Show List where
  show (List xs) = unlines . map show $ xs

instance Show Item where
  show (Incomplete str) = "- " ++ str
  show (Complete str)   = "x " ++ str

instance ToJSON List where
  toJSON (List is) = toJSON $ zipWith jsonify [0..] is

jsonify :: Int -> Item -> Value
jsonify n (Complete b)   = object ["done" .= True, "body" .= pack b, "id" .= n]
jsonify n (Incomplete b) = object ["done" .= False, "body" .= pack b, "id" .= n]

instance FromJSON Item where
  parseJSON (Object v) = case parseMaybe (.: "done") v of
                           Just True  -> Complete <$> v .: "body"
                           Just False -> Incomplete <$> v .: "body"
                           Nothing    -> fail "Couldn't parse item"
  parseJSON _          = fail "Couldn't parse item"

------------ Read ------------

parseItem :: String -> Maybe Item
parseItem ('-':' ':xs) = Just (Incomplete xs)
parseItem ('x':' ':xs) = Just (Complete xs)
parseItem _            = Nothing

parseItems :: String -> Maybe [Item]
parseItems = mapM parseItem . lines

--------- List Creation ---------

createList :: FilePath -> IO ()
createList path = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path ""

createListIfMissing :: String -> IO ()
createListIfMissing listName = do
  let path = listDirectory </> listName
  b <- doesFileExist path
  unless b $ createList path

----------- Helpers ------------

writeFile' :: FilePath -> String -> IO ()
writeFile' path contents = bracketOnError (openTempFile listDirectory "todo")
  (\(n,h) -> hClose h >> removeFile n) $ \(tempPath, tempHandle) -> do
    hPutStr tempHandle contents
    hClose tempHandle
    removeFile path
    renameFile tempPath path

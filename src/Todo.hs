{-# LANGUAGE OverloadedStrings #-}

-- | To-do lists can be represented in three different ways:
--
-- - String: a simple line-separated representation for saving lists to disk;
--
-- - List:   an internal representation;
--
-- - JSON:   a JSON representation for passing list data to the front-end.
--
-- /Disk Representation/: Each line represents a to-do, and begins either with
-- '-' to indicate an incomplete item, or 'x' to indicate a completed one. Note
-- that the list is ordered. Eg:
--
-- > - One
-- > x Two
-- > - Three
--
-- /Internal Representation/: A `List` is a list of `Item`s. Each item is
-- either a `Complete itemBody` or an `Incomplete itemBody`. Eg:
--
-- > [ Complete "One", Incomplete "Two", Complete "Three" ]
--
-- /JSON Representation/: Items in the JSON representation are explicity
-- numbered. This corresponds to the numbering implicit in the ordering of the
-- other representations. This is so a single item may be modified by the
-- front-end without having to resend the entire list. Eg:
--
-- > [
-- >   { body: "one",   done: false },
-- >   { body: "two",   done: true  },
-- >   { body: "three", done: false }
-- > ]
--
-- This module exports functions for manipulating the lists stored in
-- `listDirectory`. There's not a lot of interesting logic - just conversion
-- between the different representations, and a bunch of file IO.
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
-- | Read the contents of `listDirectory`/`listName`, parse it, and return a List
loadList :: String -> String -> IO List
loadList user listName = do
  file <- readFile $ listDirectory </> user </> listName
  case parseList file of
    Nothing    -> error "Couldn't parse list"
    Just items -> return $ List items

-- TODO: unhandled fileIO exceptions
-- | Append `item` to `listDirectory`/`listName`
addItem :: String -> String -> Item -> IO ()
addItem user listName item = do
  createListIfMissing user listName
  appendFile (listDirectory </> user </> listName) $ show item ++ "\n"

-- TODO: unhandled OOB exceptions
-- | Replace the `itemId`th item in `listDirectory`/`listName` with `item`
editItem :: String -> String -> Int -> Item -> IO ()
editItem user listName itemId item = do
  (List items) <- loadList user listName
  let (xs, _:ys) = splitAt itemId items
  writeFile' (listDirectory </> user </> listName) . show $ List (xs ++ [item] ++ ys)

-- TODO: unhandled OOB exceptions
-- | Remove the `itemId`th item from `listDirectory`/`listName`
removeItem :: String -> String -> Int -> IO ()
removeItem user listName itemId = do
  (List items) <- loadList user listName
  let (xs, _:ys) = splitAt itemId items
  writeFile' (listDirectory </> user </> listName) . show $ List (xs ++ ys)

-- | Return the names of the files in `listDirectory`.
showLists :: String -> IO [String]
showLists user = filter (notElem '.') <$> getDirectoryContents (listDirectory </> user)

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

---------- List Parsing ----------

-- TODO: consider using Parsec, or making List an instance of Read

parseItem :: String -> Maybe Item
parseItem ('-':' ':xs) = Just (Incomplete xs)
parseItem ('x':' ':xs) = Just (Complete xs)
parseItem _            = Nothing

parseList :: String -> Maybe [Item]
parseList = mapM parseItem . lines

--------- List Creation ---------

createList :: FilePath -> IO ()
createList path = do
  createDirectoryIfMissing True $ takeDirectory path
  writeFile path ""

createListIfMissing :: String -> String -> IO ()
createListIfMissing user listName = do
  let path = listDirectory </> user </> listName
  b <- doesFileExist path
  unless b $ createList path

----------- Helpers ------------

-- | Write `contents` to a temporary file in `listDirectory`, then - if nothing
-- goes wrong - rename it to `path`, overwriting any existing file at that path.
writeFile' :: FilePath -> String -> IO ()
writeFile' path contents = bracketOnError (openTempFile listDirectory "todo")
  (\(n,h) -> hClose h >> removeFile n) $ \(tempPath, tempHandle) -> do
    hPutStr tempHandle contents
    hClose tempHandle
    removeFile path
    renameFile tempPath path

{-# LANGUAGE OverloadedStrings #-}

module Auth ( Assertion (..)
            , VerifierResponse (..)
            , AuthToken (..)
            , checkAssertion
            , checkAuthToken
            , parseAuthToken
            , encryptAndSerialise
            ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Text.ParserCombinators.Parsec
import Network.HTTP.Conduit
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromChunks)
import Data.Text.Lazy (Text, unpack)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Web.ClientSession (Key, encryptIO, decrypt)
import Control.Error (hush)

data Assertion = Assertion String deriving (Show)
instance FromJSON Assertion where
  parseJSON (Object v) = Assertion <$> v .: "assertion"
  parseJSON _          = error "Invalid JSON"

data VerifierResponse = VerifierResponse String String deriving (Show)
instance FromJSON VerifierResponse where
  parseJSON (Object v) = VerifierResponse <$> v .: "status" <*> v .: "email"
  parseJSON _          = error "Invalid JSON"

data AuthToken = AuthToken String String deriving (Show)
instance FromJSON AuthToken where
  parseJSON (Object v) = AuthToken <$> v .: "email" <*> v .: "session"
  parseJSON _          = error "Invalid JSON"

checkAssertion :: ByteString -> Assertion -> IO VerifierResponse
checkAssertion hostUrl (Assertion a) = do
  url <- parseUrl "https://verifier.login.persona.org/verify"
  let req = urlEncodedBody [("audience", hostUrl), ("assertion", pack a)] url
  response <- withManager . httpLbs $ req
  -- TODO: check certificate - at present we're open to a MitM
  return . fromJust . decode . responseBody $ response

checkAuthToken :: Key -> AuthToken -> Maybe String
checkAuthToken key (AuthToken email auth) =
  case decrypt key $ pack auth of
    Nothing -> Nothing
    Just str -> if str == pack email then Just email
                                     else Nothing

encryptAndSerialise :: Key -> String -> IO Text
encryptAndSerialise key msg = do
  encMsg <- encryptIO key . pack $ msg
  return $ decodeUtf8 $ fromChunks [encMsg]

parseAuthToken :: Text -> Maybe AuthToken
parseAuthToken str = case parseCookie $ unpack str of
                       Nothing -> Nothing
                       Just c  -> AuthToken <$> lookup "email" c <*> lookup "session" c

parseCookie :: String -> Maybe [(String, String)]
parseCookie = hush . parse cookieFields "(Unknown)"

cookieFields :: GenParser Char st [(String, String)]
cookieFields = do
  first <- cookieField
  next <- (string "; " >> cookieFields) <|> return []
  return $ first:next

cookieField :: GenParser Char st (String, String)
cookieField = do
  name <- many (noneOf "=;\n")
  char '='
  value <- many (noneOf "=;\n")  
  return (name, value)

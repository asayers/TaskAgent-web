{-# LANGUAGE OverloadedStrings #-}

module Auth ( Assertion (..)
            , VerifierResponse (..)
            , AuthToken (..)
            , checkAssertion
            , encryptAndSerialise
            ) where

import Control.Applicative ((<$>), (<*>))
import Data.Aeson
import Network.HTTP.Conduit
import Data.Maybe (fromJust)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.ByteString.Lazy (fromChunks)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Web.ClientSession (Key, encryptIO)

data Assertion = Assertion String
instance FromJSON Assertion where
  parseJSON (Object v) = Assertion <$> v .: "assertion"
  parseJSON _          = error "Invalid JSON"

data VerifierResponse = VerifierResponse String String
instance FromJSON VerifierResponse where
  parseJSON (Object v) = VerifierResponse <$> v .: "status" <*> v .: "email"
  parseJSON _          = error "Invalid JSON"

data AuthToken = AuthToken String String
instance FromJSON AuthToken where
  parseJSON (Object v) = AuthToken <$> v .: "email" <*> v .: "auth"
  parseJSON _          = error "Invalid JSON"

checkAssertion :: ByteString -> Assertion -> IO VerifierResponse
checkAssertion hostUrl (Assertion a) = do
  url <- parseUrl "https://verifier.login.persona.org/verify"
  let req = urlEncodedBody [("audience", hostUrl), ("assertion", pack a)] url
  response <- withManager . httpLbs $ req
  -- TODO: check certificate - at present we're open to a MitM
  return . fromJust . decode . responseBody $ response

encryptAndSerialise :: Key -> String -> IO Text
encryptAndSerialise key msg = do
  encMsg <- encryptIO key . pack $ msg
  return $ decodeUtf8 $ fromChunks [encMsg]

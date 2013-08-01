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

-- | Identity assertion recieved from a user
data Assertion = Assertion String deriving (Show)
instance FromJSON Assertion where
  parseJSON (Object v) = Assertion <$> v .: "assertion"
  parseJSON _          = error "Invalid JSON"

-- | The validity of an assertion, as determined by the Persona verifer
data VerifierResponse = VerifierResponse { verifierStatus :: String
                                         , verifierEmail :: String
                                         } deriving (Show)
instance FromJSON VerifierResponse where
  parseJSON (Object v) = VerifierResponse <$> v .: "status" <*> v .: "email"
  parseJSON _          = error "Invalid JSON"

-- | The authentication token of a user already logged in
data AuthToken = AuthToken { tokenEmail :: String
                           , tokenSession :: String
                           } deriving (Show)
instance FromJSON AuthToken where
  parseJSON (Object v) = AuthToken <$> v .: "email" <*> v .: "session"
  parseJSON _          = error "Invalid JSON"

-- | check the validity of an assertion using the Persona verfier. Thread-blocking.
checkAssertion :: ByteString -> Assertion -> IO VerifierResponse
checkAssertion hostUrl (Assertion a) = do
  url <- parseUrl "https://verifier.login.persona.org/verify"
  let req = urlEncodedBody [("audience", hostUrl), ("assertion", pack a)] url
  response <- withManager . httpLbs $ req
  -- TODO: check certificate - at present we're open to a MitM
  return . fromJust . decode . responseBody $ response

-- | Check that an auth token was issued by the server, and that the user is who he thinks he is.
checkAuthToken :: Key -> AuthToken -> Maybe String
checkAuthToken key (AuthToken email session) =
  case decrypt key $ pack session of
    Nothing -> Nothing
    Just str -> if str == pack email then Just email
                                     else Nothing

encryptAndSerialise :: Key -> String -> IO Text
encryptAndSerialise key msg = do
  encMsg <- encryptIO key . pack $ msg
  return $ decodeUtf8 $ fromChunks [encMsg]

-- | Parse the text of a "Cookie" header, extracting an auth token if the appropriate fields are set
parseAuthToken :: Text -> Maybe AuthToken
parseAuthToken str = case parseCookie $ unpack str of
                       Nothing -> Nothing
                       Just c  -> AuthToken <$> lookup "email" c <*> lookup "session" c

-- | Parse the text of a "Cookie" header.
parseCookie :: String -> Maybe [(String, String)]
parseCookie = hush . parse cookieFields "(Unknown)"

cookieFields :: GenParser Char st [(String, String)]
cookieFields = do
  first <- cookieField
  next <- (string "; " >> cookieFields) <|> return []
  return $ first:next

cookieField :: GenParser Char st (String, String)
cookieField = do
  name <- many (noneOf "=")
  char '='
  value <- many (noneOf ";\n")  
  return (name, value)

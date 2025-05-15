{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Aeson (FromJSON, ToJSON, (.=), object, Value)
import GHC.Generics (Generic)
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Servant

-- | Request body for POST /greetme
data GreetMeRequest = GreetMeRequest
  { input :: String
  , name  :: String
  } deriving (Show, Generic)

-- | Response body for POST /greetme
data GreetMeResponse = GreetMeResponse
  { msg :: String
  } deriving (Show, Generic)

instance FromJSON GreetMeRequest
instance ToJSON GreetMeResponse

-- | API Type Definition
type API =
       "hello" :> Get '[PlainText] String
  :<|> "greet" :> Capture "name" String :> Get '[JSON] Value
  :<|> "greetme" :> ReqBody '[JSON] GreetMeRequest :> Post '[JSON] GreetMeResponse

-- | API Server Handlers
server :: Server API
server =
       helloHandler
  :<|> greetHandler
  :<|> greetMeHandler

helloHandler :: Handler String
helloHandler = return "Hello, World!"

greetHandler :: String -> Handler Value
greetHandler name = return $ object
  [ "greet" .= ("Hello" :: String)
  , "name"  .= name
  ]

greetMeHandler :: GreetMeRequest -> Handler GreetMeResponse
greetMeHandler (GreetMeRequest input name) =
  return $ GreetMeResponse (input ++ " " ++ name)

-- | API Proxy
api :: Proxy API
api = Proxy

-- | WAI Application
app :: Application
app = serve api server

-- | Main Function
main :: IO ()
main = do
  putStrLn "Server running on http://localhost:8080"
  run 8080 app

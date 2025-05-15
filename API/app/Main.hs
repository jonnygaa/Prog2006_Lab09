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
import Network.Wreq
import Network.HTTP.Conduit
import Network.HTTP.Req
import qualified Data.Text.Encoding as T
import Control.Lens
import Env (token)

{-
data Announcement = Announcement
  { date :: String  -- String for simplicity
  , title :: String
  , content :: String
  } deriving (Show, Generic)

instance ToJSON Announcement

-- | API Type Definition
type API = "" :> Get '[JSON] Announcement

-- | API Server Handlers
server :: Server API
server = response

response :: Handler Announcement
response = do
  rsp <- asJSON =<< get "https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened"
  return $ Announcement [
    "date" Data.Aeson..= ("date" :: String),
    "title" Data.Aeson..= rsp ^. title,
    "content"  Data.Aeson..= ("title" :: String)]

-- | API Proxy
api :: Servant.Proxy API
api = Servant.Proxy

-- | WAI Application
app :: Application
app = serve api server -}

-- | Main Function
main :: IO ()
main = do
  {-putStrLn "Server running on http://localhost:8081"
  run 8081 app-}
  let tok = token
      headers = Network.Wreq.header "PRIVATE-TOKEN" (T.encodeUtf8 tok)

  rsp <- req
    Servant.GET
    ( "https://git.gvk.idi.ntnu.no/api/v4/projects/5881/issues?labels=Announcement&state=opened")
    NoReqBody
    jsonResponse
    headers
--  rsp <- curl --header "PRIVATE-TOKEN`:` ${" ++ tok ++ "}" 
  putStrLn $ rsp

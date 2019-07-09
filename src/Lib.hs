{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Data.Aeson
import Data.Aeson.TH
import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import qualified Data.Map as M

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API = "users" :> Get '[JSON] [User]
        :<|> "users" :> Capture "id" Int :> Get '[JSON] ( Maybe User )

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return allUser :<|> maybeUser 
     where maybeUser :: Int -> Handler (Maybe User)
           maybeUser id = return $ M.lookup id  users     

isaac :: User
isaac = User 1 "Isaac" "Newton"

albert :: User 
albert = User 2 "Albert" "Einstein"

allUser :: [User]
allUser = snd <$> M.toList users

users :: M.Map Int User
users = M.fromList [(1, isaac), (2, albert)]

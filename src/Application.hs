{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Application
  ( startApp
  , app
  ) where

import           Network.Wai
import           Network.Wai.Handler.Warp
import           Processor.Simple
import           Servant

type API = SimpleAPI

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = processHandler

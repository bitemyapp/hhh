{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import           Data.Swagger
import           Data.Text
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.Swagger

type HelloAPI = "hello" :> Get '[JSON] Text

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type TestAPI = SwaggerAPI :<|> HelloAPI

server :: Server TestAPI
server = return (toSwagger (Proxy :: Proxy HelloAPI))
    :<|> return "Hello, world!"

runServer :: IO ()
runServer = run 8000 $ simpleCors $ logStdoutDev $ serve (Proxy :: Proxy TestAPI) server

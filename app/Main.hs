{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Servant
import Data.Text
import Data.Swagger
import Servant.Swagger
import Network.Wai.Middleware.Cors (simpleCors)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

type HelloAPI = "hello" :> Get '[JSON] Text

type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger

type TestAPI = SwaggerAPI :<|> HelloAPI

server :: Server TestAPI
server = return (toSwagger (Proxy :: Proxy HelloAPI))
    :<|> return "Hello, world!"

main :: IO ()
main = run 8000 $ simpleCors $ logStdoutDev $ serve (Proxy :: Proxy TestAPI) server

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE TypeOperators     #-}

module Lib where

import           Data.Aeson                           (ToJSON)
import           Data.ByteString                      (ByteString)
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy                 as BL
import           Data.Map                             (Map, fromList)
import qualified Data.Map                             as Map
import           Data.Semigroup                       ((<>))
import           Data.Swagger
import           Data.Text
import           Data.Time.Clock
import           GHC.Generics                         (Generic)
import           Network.Wai                          (Request, requestHeaders)
import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.Cors          (simpleCors)
import           Network.Wai.Middleware.RequestLogger (logStdoutDev)
import           Servant
import           Servant.Server.Experimental.Auth     (AuthHandler,
                                                       AuthServerData,
                                                       mkAuthHandler)
import           Servant.Server.Experimental.Auth     ()
import           Servant.Swagger
import           System.IO.Unsafe
import           Web.Cookie

------------------------ mucking with cookies --------------------

sc :: SetCookie
sc = def { setCookieName = "a"
         , setCookieValue = "b"
         , setCookiePath = Just "/"
         , setCookieExpires = Just (unsafePerformIO getCurrentTime)
         , setCookieMaxAge = Just (secondsToDiffTime 9001)
         , setCookieDomain = Just "localhost"
         , setCookieHttpOnly = True --- these are usually both false
         , setCookieSecure = True   --- this is inconsistent/invalid, is it not?
         , setCookieSameSite = Just sameSiteStrict
         }

-- Maximum Cookie
-- "a=b; Path=/; Expires=Sun, 24-Jul-2016 04:39:34 GMT; Max-Age=9001; Domain=localhost; HttpOnly; Secure; SameSite=Strict"

scBs :: BL.ByteString
scBs = toLazyByteString $ renderSetCookie sc

------------------------ end cookies, heaven forbid --------------

------------------------ AUTH STUFF ------------------------------

-- | A user type that we "fetch from the database" after
-- performing authentication
newtype Account = Account { unAccount :: Text }

-- | A (pure) database mapping keys to users.
database :: Map ByteString Account
database = fromList [ ("key1", Account "Anne Briggs")
                    , ("key2", Account "Bruce Cockburn")
                    , ("key3", Account "Ghédalia Tazartès")
                    ]

-- | A method that, when given a password, will return a Account.
-- This is our bespoke (and bad) authentication logic.
lookupAccount :: ByteString -> Handler Account
lookupAccount key = case Map.lookup key database of
  Nothing -> throwError (err403 { errBody = "Invalid Cookie" })
  Just usr -> return usr

-- | The auth handler wraps a function from Request -> Handler Account
-- we look for a Cookie and pass the value of the cookie to `lookupAccount`.
authHandler :: AuthHandler Request Account
authHandler =
  let handler req = case lookup "servant-auth-cookie" (requestHeaders req) of
        Nothing -> throwError (err401 { errBody = "Missing auth header" })
        Just authCookieKey -> lookupAccount authCookieKey
  in mkAuthHandler handler

-- | Our API, with auth-protection
type AuthGenAPI = "private" :> AuthProtect "cookie-auth" :> PrivateAPI
             :<|> "public"  :> PublicAPI

-- | A value holding our type-level API
genAuthAPI :: Proxy AuthGenAPI
genAuthAPI = Proxy

-- | We need to specify the data returned after authentication
type instance AuthServerData (AuthProtect "cookie-auth") = Account

-- | The context that will be made available to request handlers. We supply the
-- "cookie-auth"-tagged request handler defined above, so that the 'HasServer' instance
-- of 'AuthProtect' can extract the handler and run it on the request.
genAuthServerContext :: Context (AuthHandler Request Account ': '[])
genAuthServerContext = authHandler :. EmptyContext

-- | Our API, where we provide all the author-supplied handlers for each end
-- point. Note that 'privateDataFunc' is a function that takes 'Account' as an
-- argument. We dont' worry about the authentication instrumentation here,
-- that is taken care of by supplying context
genAuthServer :: Server AuthGenAPI
genAuthServer =
  let privateDataFunc (Account name) =
          return (PrivateData ("this is a secret: " <> name))
      publicData = return [PublicData "this is a public piece of data"]
  in  privateDataFunc :<|> publicData

-- | run our server
genAuthMain :: IO ()
genAuthMain = run 8080 (serveWithContext genAuthAPI genAuthServerContext genAuthServer)

{- Sample Session:

$ curl -XGET localhost:8080/private
Missing auth header

$ curl -XGET localhost:8080/private -H "servant-auth-cookie: key3"
[{"ssshhh":"this is a secret: Ghédalia Tazartès"}]

$ curl -XGET localhost:8080/private -H "servant-auth-cookie: bad-key"
Invalid Cookie

$ curl -XGET localhost:8080/public
[{"somedata":"this is a public piece of data"}]
-}

------------------ END AUTH STUFF --------------------------------

-- | private data that needs protection
newtype PrivateData = PrivateData { ssshhh :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PrivateData

-- | public data that anyone can use.
newtype PublicData = PublicData { somedata :: Text }
  deriving (Eq, Show, Generic)

instance ToJSON PublicData

-- | A user we'll grab from the database when we authenticate someone
newtype User = User { userName :: Text }
  deriving (Eq, Show)

-- | a type to wrap our public api
type PublicAPI = Get '[JSON] [PublicData]

-- | a type to wrap our private api
type PrivateAPI = Get '[JSON] PrivateData


type HelloAPI = "hello" :> Get '[JSON] Text
type SwaggerAPI = "swagger.json" :> Get '[JSON] Swagger
type TestAPI = SwaggerAPI :<|> HelloAPI

server :: Server TestAPI
server = return (toSwagger (Proxy :: Proxy HelloAPI))
    :<|> return "Hello, world!"

runServer :: IO ()
runServer = run 8000 $ simpleCors $ logStdoutDev $ serve (Proxy :: Proxy TestAPI) server

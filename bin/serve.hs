{-# LANGUAGE OverloadedStrings #-} -- For hard-coded key.

module Main (main) where

import qualified Parse
import qualified Prototype.Runtime as Rt
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Auth.Server

-- For hard-coded key.
import Crypto.JOSE.JWK (fromKeyMaterial)
import Crypto.JOSE.JWA.JWK (KeyMaterial(OctKeyMaterial), OctKeyParameters(..))
import Crypto.JOSE.Types (Base64Octets(..))

import qualified Options.Applicative as A
import qualified Prototype.Runtime.StmDatabase as Database (newHandle)
import Prototype.Server (api, server)


--------------------------------------------------------------------------------
port :: Int
port = 7249


--------------------------------------------------------------------------------
main :: IO ()
main = do
  confInMode <- A.execParser Parse.parseFullConf
  putStrLn @Text $ "Starting up: " <> show confInMode
  -- This could be taken form disk, or hard-coded here, to keep existing
  -- sessions alive.
  key <- generateKey
  -- let key = fromKeyMaterial (OctKeyMaterial (OctKeyParameters (Base64Octets "aa")))

  -- Disable XSRF Cookie (otherwise, this needs some logic instead of
  -- simple cURL calls):
  -- https://github.com/haskell-servant/servant-auth/issues/55#issuecomment-747046527
  -- Using Secure with Same-Site=Strict would be good enough ?
  let cookieSettings = defaultCookieSettings
        { cookieIsSecure = NotSecure
          -- ^ Use temporarily NotSecure for easier local testing with cURL.
        , cookieXsrfSetting = Nothing
        , cookieSameSite = SameSiteStrict
        }

      jwtSettings = defaultJWTSettings key

      settings = cookieSettings :. jwtSettings :. EmptyContext

  database <- Database.newHandle

  run port $ serveWithContext api settings $
    server database cookieSettings jwtSettings

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens
import Control.Monad.IO.Class
import Data.Aeson
import Data.Aeson.Lens
import Network.Google as Google
import Network.Google.Auth
import Network.Google.AppsCalendar as Calendar
import System.IO
import System.Environment
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.IO as T

loadTokenFile :: FilePath -> (Credentials s -> IO ()) -> IO (Credentials s)
loadTokenFile path sav = go . decode =<< BS.readFile path where
  go json = do
    let cli = OAuthClient (json ^. key "client_id" ^?! _Just) (json ^. key "client_secret" ^?! _Just)
    T.putStrLn $ formURL cli calendarScope
    code <- OAuthCode <$> T.getLine
    let cred = installedApplication cli code
    sav cred
    return cred

main :: IO ()
main = do
  lgr <- newLogger Debug stdout
  mgr <- newManager tlsManagerSettings
  args <- getArgs
  let clientJSONpath = "token/client.json"
  let userJSONpath = "token/test.json"
  
  cred <- case args of
    ("auth":_) -> loadTokenFile clientJSONpath (\cred -> saveAuthorizedUser userJSONpath True . (\(Right r) -> r) =<< fmap authToAuthorizedUser . retrieveAuthFromStore =<< initStore cred lgr mgr)
    _ -> fromFilePath userJSONpath
    
  env <- newEnvWith cred lgr mgr <&> envScopes .~ calendarScope

  runResourceT . runGoogle env $ timeout (Seconds 3600) $ do
    p <- send calendarListList
    liftIO $ print p
    
  putStrLn "hello world"



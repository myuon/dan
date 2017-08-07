{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding ((.=), argument)
import Data.Aeson
import Data.Aeson.Lens
import System.IO
import qualified Data.ByteString.Lazy as BS
import Options.Applicative
import System.Directory
import Data.Time
import Data.Semigroup ((<>))

{-
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
-}

data DanItem
  = DanItem
  { _doneAt :: UTCTime
  , _itemName :: String
  } deriving (Eq, Show)

makeLenses ''DanItem

instance ToJSON DanItem where
  toJSON (DanItem d i) = object ["done_at" .= d, "item_name" .= i]

instance FromJSON DanItem where
  parseJSON (Object v) = DanItem <$> (v .: "done_at") <*> (v .: "item_name")

danDir = "dan"
jsonFile = "201808.json"
jsonPath = danDir ++ "/" ++ jsonFile

main = do
  createDirectoryIfMissing True danDir

  j <- doesFileExist jsonPath >>= \case
    True -> BS.readFile jsonPath
    False -> return "[]"
  parseOptions >>= execDan ((\(Just s) -> s) $ decode @[DanItem] j)

data DanOption
  = DAdd String (Either String ())
  | DList Bool
  deriving (Eq, Show)

execDan :: [DanItem] -> DanOption -> IO ()
execDan j (DAdd name (Right ())) = do
  time <- getCurrentTime
  BS.writeFile jsonPath $ encode (DanItem time name : j)
execDan j (DAdd name (Left ftime)) = do
  let time = parseTimeOrError True defaultTimeLocale "%Y-%m-%d" ftime
  BS.writeFile jsonPath $ encode (DanItem time name : j)
    
parseOptions :: IO DanOption
parseOptions = execParser opts where
  opts = info (danParser <**> helper) $
    fullDesc
    <> progDesc "`DONE` management tool"
    <> header "dan - `DONE` management tool"

  danParser :: Parser DanOption
  danParser = subparser
    $ command "add" (info padd $ progDesc "Add a new `DONE` item")
    <> command "list" (info plist $ progDesc "List current jobs")

    where
      padd = DAdd
        <$> argument str (metavar "ITEM_NAME")
        <*> (fmap Left pdate <|> pure (Right ()))

        where
          pdate = strOption
            $ long "date"
            <> short 'd'
            <> help "When have you done this?"
            <> metavar "YYYY-MM-DD"

      plist = fmap DList $ switch
        $ long "list"
        <> short 'l'
        <> help "List current jobs"

  

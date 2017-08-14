{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Lens hiding ((.=), argument)
import Control.Monad
import Control.Monad.Reader (ReaderT, MonadIO)
import Control.Monad.Logger (NoLoggingT)
import Control.Monad.Trans.Resource (ResourceT, MonadBaseControl)
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Map as M
import Data.List.Split
import System.IO
import System.Process
import Options.Applicative
import System.Directory
import Data.Time
import Data.Semigroup ((<>))
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Database.Persist.Sqlite

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
DanJob json
  name T.Text

DanItem json
  job DanJobId
  doneAt UTCTime
  comment T.Text
|]

danDir = "persist"
dbFile = "test.sqlite"
dbPath = danDir ++ "/" ++ dbFile

runDB :: (MonadBaseControl IO m, MonadIO m) => ReaderT SqlBackend (NoLoggingT (ResourceT m)) a -> m a
runDB = runSqlite (T.pack dbPath)

fetchJobId :: DanJob -> IO DanJobId
fetchJobId job = do
  runDB $ do
    xs <- selectList [DanJobName ==. danJobName job] [LimitTo 1]
    case xs of
      [] -> insert job
      (Entity key _:_) -> return key

main = do
  doesPathExist dbPath >>= \b -> when (not b) $ do
    createDirectoryIfMissing True danDir
    runSqlite (T.pack dbPath) $ runMigration migrateAll

  parseOptions >>= execDan

data DanOption
  = DAdd String (Either String ()) String
  | DList
  | DGitLog FilePath
  | DGraph
  deriving (Eq, Show)

execDan :: DanOption -> IO ()
execDan (DAdd n t c) = do
  time <- either (return . parseTimeOrError True defaultTimeLocale "%Y-%m-%d") (\_ -> getCurrentTime) t
  job <- case n of
    ('#':nid) -> do
      jobs <- runDB $ selectList [] []
      return $ (\(Entity k _) -> k) $ jobs !! read nid
    _ -> fetchJobId (DanJob (T.pack n))
  runDB $ insert_ $ DanItem job time (T.pack c)
execDan DList = do
  jobs <- runDB $ selectList [] [] :: IO [Entity DanJob]
  putStrLn "== registered jobs =="
  mapM_ (\(i,v) -> putStrLn $ "#" ++ show i ++ ": " ++ show (encode v)) $ zip [0..] $ fmap (\(Entity _ v) -> v) jobs
execDan (DGitLog repo) = do
  xs <- readCreateProcess (shell $ "git --git-dir=" ++ repo ++ "/.git log --date=iso --pretty=format:\"%h %cd\" | awk '{print $1\" \"$2}'") ""
  job <- fetchJobId (DanJob "git-commit")

  runDB $ insertMany_ $ fmap (\[hash,date] -> DanItem job (parseTimeOrError True defaultTimeLocale "%Y-%m-%d" date) (T.pack hash)) $ fmap words $ lines xs
execDan DGraph = do
  ds <- fmap (fmap (\(Entity _ v) -> v)) $ runDB $ selectList [] []
  let mp = foldl (\mp i -> M.insertWith (\_ -> (i :)) (utctDay (danItemDoneAt i)) [i] mp) M.empty ds

  mapM_ (\s -> putStrLn $ "|" ++ s ++ "|") =<< densityGraph mp

  putStrLn "\n== latest commits =="
  mapM_ (BS.putStrLn . encode) $ take 10 $ concat $ reverse $ M.elems mp
  
densityGraph :: M.Map Day [DanItem] -> IO [String]
densityGraph mp = do
  let maxDensity = maximum $ M.elems $ fmap length mp
  cur <- getCurrentTime

  forM (chunksOf 30 [addDays (-100) (utctDay cur) .. utctDay cur]) $ \days -> do
    forM days $ \day -> do
      return $ graphChar (maybe 0 length $ M.lookup day mp) maxDensity

  where
    graphChar :: Int -> Int -> Char
    graphChar i maxi | 0 == i = ' '
    graphChar i maxi | 0 < i && i <= maxi `div` 3 = '░'
    graphChar i maxi | maxi `div` 3 < i && i <= (maxi `div` 3) * 2 = '▒'
    graphChar i maxi | (maxi `div` 3) * 2 < i = '▓'

parseOptions :: IO DanOption
parseOptions = execParser opts where
  opts = info (danParser <**> helper) $
    fullDesc
    <> progDesc "`DONE` management tool"
    <> header "dan - `DONE` management tool"

  danParser :: Parser DanOption
  danParser = subparser
    ( command "add" (info padd $ progDesc "Add a new `DONE` item")
    <> command "list" (info plist $ progDesc "List current jobs")
    <> command "load" (info pload $ progDesc "List current jobs")
    )
    <|> pure DGraph

    where
      padd = DAdd
        <$> argument str (metavar "ITEM_NAME")
        <*> (fmap Left pdate <|> pure (Right ()))
        <*> (strOption (long "comment" <> short 'c' <> metavar "COMMENT") <|> pure "")

        where
          pdate = strOption
            $ long "date"
            <> short 'd'
            <> help "When have you done this?"
            <> metavar "YYYY-MM-DD"

      plist = pure DList

      pload = DGitLog
        <$> strOption (long "git" <> short 'g' <> metavar "GIT_REPO" <> help "Load commits from given git repository")


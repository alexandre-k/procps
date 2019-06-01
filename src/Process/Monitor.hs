{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Process.Monitor
  ( MonitoredProcess (..)
  , listAll
  , monitoredProcess
  , startM
  )
where

import qualified Process.Internal.Common as Internal
import Process.Manage (Process(..), isAlive, kill, start)
import Control.Exception
import Control.Monad as M
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as C
import qualified Data.ByteString.Lazy as B
import qualified Data.String.Utils as U
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import GHC.Generics
import System.Directory
import System.Exit
import System.FilePath.Posix
import qualified System.IO.Strict as S
import System.Process (getPid, shell, createProcess)
import qualified System.Process.Typed as P


data MonitoredProcess = MonitoredProcess
  { name        :: T.Text
  , process     :: Process
  , memory      :: Float
  , cpu         :: Float
  , uptime      :: Int
  , status      :: T.Text
  -- , logFile     :: FilePath
  } deriving (Generic, Show, ToJSON, FromJSON)


processState :: Process -> IO String
processState p = do
  alive <- isAlive p
  return $ if alive then "Started" else "Stopped"


-- start a process as a monitored process given a unique name and a
-- command to start it
monitoredProcess :: T.Text -> T.Text -> IO (Maybe MonitoredProcess)
monitoredProcess name cmd = do
  startedProcess <- start name cmd
  loggingDirectory <- Internal.loggingDirectory
  case startedProcess of
    Just p -> do
      state <- processState p
      cpu <- cpuUsage p
      memory <- memoryUsage p
      uptime <- elapsedTime p
      return $ Just MonitoredProcess { name        = name
                                     , process     = p
                                     , memory      = memory
                                     , cpu         = cpu
                                     , uptime      = round uptime
                                     , status      = T.pack state
                                     -- , logFile     = loggingDirectory </> T.unpack name
                                     }
    Nothing -> do
      return Nothing


psInfo :: String -> IO (Float)
psInfo cmd = do
  (_, output)<- P.readProcessStdout $ P.shell cmd
  let val = read (U.strip . C.unpack $ output) :: Float
    in
    return val

cpuUsage :: Process -> IO Float
cpuUsage p = psInfo ("ps -p " ++ (pid p) ++ " -o %cpu | grep -v CPU")

memoryUsage :: Process -> IO Float
memoryUsage p = psInfo ("ps -p " ++ (pid p) ++ " -o %mem | grep -v MEM")

elapsedTime :: Process -> IO Float
elapsedTime p = psInfo ("ps -p " ++ (pid p) ++ " -o etimes | grep -v ELAPSED")


startM :: T.Text -> T.Text -> IO (Maybe MonitoredProcess)
startM name cmd = do
  mprocesses <- listAll
  unique <- isUnique name mprocesses
  M.guard $ unique
  mprocess <- monitoredProcess name cmd
  case mprocess of
    Just p -> do
      isAlive (process p)
      save $ p:mprocesses
      return $ Just p
    Nothing -> do
      return Nothing
  where
    isUnique :: T.Text -> [MonitoredProcess] -> IO Bool
    isUnique processName mprocesses = do
      return $ not $ processName `elem` (processesNames mprocesses)


processesNames :: [MonitoredProcess] -> [T.Text]
processesNames procs = map (\p -> name p) procs


listAll :: IO [MonitoredProcess]
listAll = do
  confFile <- Internal.configFile
  mprocesses <- S.readFile confFile
  let mprocesses' = decode . C.pack $ mprocesses
  case mprocesses' of
    Just dprocess -> do
      return dprocess
    Nothing -> do
      return []




  -- processes <- try $ withFile confFile ReadMode B.hGetContents
  -- case processes of
  --   Left (_ :: IOException) -> return []
  --   Right processes' -> do
  --     case decode processes' of
  --       Just dprocess -> return dprocess
  --       Nothing -> return []



save :: [MonitoredProcess] -> IO ()
save processes = do
  confFile <- Internal.configFile
  saved <- try $ C.writeFile confFile (encode $ processes)
  case saved of
    Left (err :: IOException) -> putStrLn $ "Error " ++ show err
    Right success -> putStrLn $ "Saved " ++ show success

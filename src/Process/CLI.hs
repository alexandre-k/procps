{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.CLI
  ( parse
  , parseOptions
  , withInfo
  )
where

import Control.Monad as M
import Data.Semigroup ((<>))
import Data.String (fromString)
import qualified Data.Text as T
import Options.Applicative
import qualified Process.Internal.Common as Internal
import qualified Process.Manage as MA
import qualified Process.Monitor as MO
import Process.Web.Server
import System.Directory
import System.Exit
import Text.Tabular
import Text.Tabular.AsciiArt


data Options
  = Serve String Int
  | Start String String
  | Stop FilePath
  | ListAll
  | Show String


showParser :: Parser Options
showParser = Show <$> strArgument
      ( metavar "process name"
      <> help "Show a monitored process given its unique identifier")

listAllParser :: Parser Options
listAllParser = pure ListAll

startParser :: Parser Options
startParser = Start
  <$> strArgument
  ( metavar "process name"
  <> help "Name of a process existing in /usr/bin")
  <*> strArgument
  ( metavar "command of the said process"
  <> help "A command to launch a given process")

stopParser :: Parser Options
stopParser = Stop <$> strArgument
  ( metavar "Name of a process"
  <> help "Name of a process")

serveParser :: Parser Options
serveParser = Serve
  <$> strOption
      ( long "ip"
      <> short 'i'
      <> metavar "x.x.x.x"
      <> value "127.0.0.1"
      <> help "IP address used for serving files"
      )
  <*> option auto
      ( long "port"
      <> short 'p'
      <> metavar "int"
      <> value 5000
      <> help "Port used to listen to connections"
      )

parseOptions :: Parser Options
parseOptions = subparser $
  command "serve"
   (withInfo serveParser "Start a web server to visualize processes through a web interface")
  <> command "start"
   (withInfo startParser "Start a process given its name and a command")
  <> command "stop"
   (withInfo stopParser "Stop a process given its name")
  <> command "list"
   (withInfo listAllParser "List all started processes")
  <> command "show"
   (withInfo showParser "Show a process given its name")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

pInfo :: MO.MonitoredProcess -> [String]
pInfo mprocess =
  [T.unpack $ MO.name mprocess
  , show $ MA.command p
  , show $ MA.pid p
  , T.unpack $ MO.status mprocess
  , show $ MO.memory mprocess
  , show $ MO.cpu mprocess
  , show $ MO.uptime mprocess
  ]
  where
    p = MO.process mprocess


parse :: Options -> IO ()
parse command = do
  -- confFile <- Internal.configFile
  -- configDirectory <- Internal.configDirectory
  -- createDirectoryIfMissing True configDirectory
  -- confFileExists <- doesFileExist confFile
  -- M.when (not confFileExists) (writeFile confFile "")

  case command of

    Serve ip port -> do
      putStrLn $ "Launch server: " ++ ip ++ ":" ++ show port
      serve (Server ip port)

    Start name cmd -> do
      mprocess <- (MO.startM (T.pack name) (T.pack cmd))
      case mprocess of
        Just p -> do
          print $ show p
        Nothing -> do
          putStrLn $ "Unable to start process"

    Stop name -> do
      mprocesses <- MA.findProcess MA.PName (T.pack name)
      let mprocess = head mprocesses
      exitCode <- MA.stop MA.Process { MA.pname   = MA.pname mprocess
                                     , MA.pid     = MA.pid mprocess
                                     , MA.command = MA.command mprocess
                                     }
      case exitCode of
        ExitFailure _ -> putStrLn $ "No process " ++ name ++ " found!"
        _ -> putStrLn $ name ++ " stopped."

    ListAll -> do
      mprocesses <- MO.listAll
      showProcesses mprocesses

    Show name -> do
      mprocesses <- MA.findProcess MA.PName (T.pack name)
      putStrLn $ show mprocesses


showProcesses :: [MO.MonitoredProcess] -> IO ()
showProcesses [] = putStrLn "No processes currently monitored."
showProcesses mprocesses = putStrLn $ render id id id pTable
  where
  pTable = Table
      (Group NoLine (map (\n -> Header (show n)) [1..(length mprocesses)]))
      (Group NoLine [ Header "name"
                  , Header "command"
                  , Header "pid"
                  , Header "status"
                  , Header "memory"
                  , Header "cpu"
                  , Header "uptime"
                  ])
      (map pInfo mprocesses)

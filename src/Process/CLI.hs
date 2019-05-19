{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.CLI
  ( parse
  , parseCommand
  , withInfo
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Process.Web.Server

data Command
  = Serve String Int
  | Start String

  -- , show :: String
  -- , listAll :: Bool
  -- , create :: String
  -- , find :: String


-- app :: Parser Manager
-- app = Manager
--   <$> strOption
--       ( long "start-server"
--       <> metavar "[ip address]:port"
--       <> help "Start a web server to visualize processes through a web interface"
--       )
--   <*> strOption
--       ( long "show"
--       <> metavar "process name"
--       <> help "Show a monitored process given its unique identifier"
--       )
--   <*> switch
--       ( long "list-all"
--       <> help "Show all monitored processes"
--       )
--   <*> strOption
--       ( long "create"
--       <> metavar "command to run a process"
--       <> help "Create a process managed through the API or the CLI"
--       )
--   <*> strOption
--       ( long "find"
--       <> metavar "name of a process"
--       <> help "Find a process given its name"
--       )

-- serve :: Parser Command
-- serve = fmap S.Server serverParser

start :: Parser Command
start = Start
  <$> strOption
      ( long "process"
      <> short 'p'
      <> metavar "name"
      <> value ""
      <> help "Name of a process you want to start"
      )

serveParser :: Parser Command
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
      <> value 3000
      <> help "Port used to listen to connections"
      )

parseCommand :: Parser Command
parseCommand = subparser $
  command "serve"
   (withInfo serveParser "Start a web server to visualize processes through a web interface")
  <> command "start"
   (withInfo start "Start a process given a name")


withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parse :: Command -> IO ()
parse command =
  case command of
    Serve _ _ -> putStrLn "Launch server"
    Start _ -> putStrLn "Start process"

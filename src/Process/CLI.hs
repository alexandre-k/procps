{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.CLI where

import Data.Semigroup ((<>))
import Options.Applicative
import Process.Web.Server as S



data Command =
  StartServer
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

server :: Parser S.Server
server = S.Server
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

parseServer :: Parser S.Server
parseServer = subparser $
  command "start-server" (withInfo server "Start a web server to visualize processes through a web interface")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parse :: S.Server -> IO ()
parse server = S.start server

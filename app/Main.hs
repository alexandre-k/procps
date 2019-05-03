#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import qualified Process.Information as PI
import qualified Process.Manage as PM
import qualified Process.Monitor as PMO
import qualified Process.Resources as R
import Data.Semigroup ((<>))
import Options.Applicative
import Process.Web.Server as S


data Command =
  StartServer
  -- , show :: String
  -- , listAll :: Bool
  -- , create :: String
  -- , find :: String

data Server = Server
  { ip :: String
  , port :: Int
  } deriving (Show)

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

server :: Parser Server
server = Server
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

parseCommand :: Parser Server
parseCommand = subparser $
  command "start-server" (withInfo server "Start a web server to visualize processes through a web interface")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parse :: Server -> IO ()
parse cmd = print cmd

main :: IO ()
main = do
  parse =<< execParser (withInfo parseCommand "Command line application to parse processes")

-- main = do
--   -- P.start "kate"
--   PMO.create "thunderbird" "thunderbird"
--   kate <- PM.findProcess PM.PName "thunderbird"
--   putStrLn $ show kate
--   environ <- PI.listProcessEnviron "3654"
--   putStrLn $ show environ
--   cwd <- PI.seeCwd "3654"
--   putStrLn cwd

--   -- result <- P.kill $ kate !! 0
--   -- putStrLn $ show result
--   -- p <- processes
--   -- print p

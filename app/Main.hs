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

-- start server
-- create process
-- show cpu usage of a process
-- show all processes with list all

data Manager = Manager
  { start :: String
  , show :: String
  , listAll :: Bool
  , create :: String
  , find :: String
  } deriving (Show)

app :: Parser Manager
app = Manager
  <$> strOption
      ( long "start-server"
      <> metavar "[ip address]:port"
      <> help "Start a web server to visualize processes through a web interface"
      )
  <*> strOption
      ( long "show"
      <> metavar "process name"
      <> help "Show a monitored process given its unique identifier"
      )
  <*> switch
      ( long "list-all"
      <> help "Show all monitored processes"
      )
  <*> strOption
      ( long "create"
      <> metavar "command to run a process"
      <> help "Create a process managed through the API or the CLI"
      )
  <*> strOption
      ( long "find"
      <> metavar "name of a process"
      <> help "Find a process given its name"
      )

parse :: Manager -> IO ()
parse cmd = print cmd

main :: IO ()
main = do
  parse =<< execParser options
  where
    options = info (app <**> helper)
      (fullDesc
       <> progDesc "Command line application to parse processes"
       <> header "Targeted for Linux, BSD and Windows platforms")

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

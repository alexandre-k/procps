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
  -- , show :: String
  -- , create :: [String]
  -- , find :: String
  } deriving (Show)

app :: Parser Manager
app = Manager
  <$> strOption
      ( long "start-server"
      <> metavar "[ip address]:port"
      <> help "Start a web server to visualize processes through a web interface"
      )

parse :: Manager -> IO ()
parse cmd = S.start

main :: IO ()
main = do
  parse =<< execParser options
  where
    options = info (app <**> helper)
      (fullDesc
       <> progDesc "Command line application to parse processes"
       <> header "Targeted for Linux, BSD and Windows platforms")
    -- options :: Parser Manager
    -- options = Manager <$>
    --   <> command "show" (info app (progDesc "Show a monitored process given its unique identified"))
    --   <> command "list-all" (info app (progDesc "Show all monitored processes"))
    --   <> command "create" (info app (progDesc "Create a process managed through the API or the CLI"))
    --   <> command "find" (info app (progDesc "Find a process given its name"))
    --   <> commandGroup "Process anagement commands:"
    --   <> hidden
    --   )


-- main = do
--   -- P.start "kate"
--   PMO.create "thunderbird" "thunderbird"
--   cpuUsage <- R.cpuUsage
--   putStrLn $ show cpuUsage
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

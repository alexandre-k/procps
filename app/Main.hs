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
import Process.Web.Server

-- start server
-- create process
-- show cpu usage of a process
-- show all processes with list all
data Options = Options
  { optCommand :: !Server
  }

data Manage = Create String String
data Info = Find String
data Server = Start String | Stop


main :: IO ()
main = do
  (opts :: Options) <- execParser optsParser
  case optCommand opts of
    Start ipAddr -> start
  where
    optsParser :: ParserInfo Options
    optsParser =
      info
        (helper <*> options)
        (fullDesc <> progDesc "Command line application to parse processes" <>
         header
           "Targeted for Linux, BSD and Windows platforms")
    options :: Parser Options
    options =
      Options <$> switch (long "flag" <> help "flag help message") <*>
      hsubparser (start <> stop)
    start =
      command "start server" (info "start server")
    stop =
      command "stop" (info (pure Stop) (progDesc "stop something"))



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

#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariable #-}
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

data Manage = Create String String | Stop String
data Info = Find String
data Server = Start String | Stop


main :: IO ()
main = do
  (opts :: Options) <- execParser optsParser
  case optCommand opts of
    Start ipAddr -> start ipAddr
  where
    optsParser :: ParserInfo Options
    optsParser =
      info
        (fullDesc <> progDesc "Command line application to parse processes" <>
         header
           "Targeted for Linux, BSD and Windows platforms")



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

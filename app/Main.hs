{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import qualified Process.Information as PI
import qualified Process.Manage as PM
import qualified Process.Resources as R


main = do
  -- P.start "kate"
  PM.createMonitoredProcess "firefox" "firefox"
  cpuUsage <- R.cpuUsage
  putStrLn $ show cpuUsage
  kate <- PM.findProcess PM.PName "thunderbird"
  putStrLn $ show kate
  environ <- PI.listProcessEnviron "3654"
  putStrLn $ show environ
  cwd <- PI.seeCwd "3654"
  putStrLn cwd

  -- result <- P.kill $ kate !! 0
  -- putStrLn $ show result
  -- p <- processes
  -- print p

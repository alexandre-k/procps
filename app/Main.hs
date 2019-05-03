#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Options.Applicative
import Process.CLI (withInfo, parseServer, parse)

main :: IO ()
main = do
  parse =<< execParser (withInfo parseServer "Command line application to parse processes")

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

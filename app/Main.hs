#!/usr/bin/env stack
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Options.Applicative
import Process.CLI (withInfo, parseOptions, parse)

main :: IO ()
main = do
  parse =<< execParser (withInfo parseOptions "Command line application to parse processes")

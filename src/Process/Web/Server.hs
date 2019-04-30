{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server where

import Web.Scotty
import Data.Monoid (mconcat)

main = scotty 3000 $
  get ":/processes" $ do
  html $ mconcat ["<h1>Scotty, beam me up!</h1>"]

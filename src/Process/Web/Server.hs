{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Server where

import Web.Scotty
import Data.Default.Class (def)
import Data.Monoid (mconcat)
import qualified Network.Wai.Handler.Warp as W


-- customSettings :: String -> Option
customSettings ipAddr = def { verbose = 1
                     , settings = W.setHost ipAddr $ settings def
                     }


start :: IO ()
start ipAddr = scottyOpts $ customSettings ipAddr $ do
  get "/" $ text "hello world"

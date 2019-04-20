{-# LANGUAGE OverloadedStrings #-}

module Process.Information
  ( listProcessEnviron
  , seeCwd
  )
where

import qualified Process.Internal.Common as Internal
import qualified Data.ByteString.Lazy.Char8 as C8
import Data.String.Utils
import qualified System.Process.Typed as P


-- list all environment variables used to run a process
formatEnviron :: String -> [(String, String)]
formatEnviron environ = map (splitAtFirst "=") ((split "\0") environ)

-- helper function to show environment variables. Split keys and values at "="
splitAtFirst :: String -> String -> (String, String)
splitAtFirst _ [] = ("", "")
splitAtFirst sep str =
  (key, value)
  where
    keyValues = split sep str
    key = head keyValues
    value = join "=" (tail keyValues)

-- get environment variables used for a given process
listProcessEnviron :: String -> IO [(String, String)]
listProcessEnviron process = do
  environ <- readFile $ Internal.processEnviron process
  return $ formatEnviron environ

-- show the current working directory of a given process
seeCwd :: String -> IO String
seeCwd process = do
  (out, _) <- P.readProcess_ $ P.shell $ "readlink " ++ (Internal.processCwd process)
  return $ strip $ C8.unpack $ out

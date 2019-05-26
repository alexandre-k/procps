{-# LANGUAGE OverloadedStrings #-}

module Process.Information
  ( listProcessEnviron
  , seeCwd
  )
where

import qualified Process.Internal.Common as Internal
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified System.Process.Typed as P


-- list all environment variables used to run a process
formatEnviron :: T.Text -> [(T.Text, T.Text)]
formatEnviron environ = map (splitAtFirst "=") ((T.splitOn "\0") environ)

-- helper function to show environment variables. Split keys and values at "="
splitAtFirst :: T.Text -> T.Text -> (T.Text, T.Text)
splitAtFirst _ "" = ("", "")
splitAtFirst sep str =
  (key, value)
  where
    keyValues = T.splitOn sep str
    key = head keyValues
    value = T.intercalate "=" (tail keyValues)

-- get environment variables used for a given process
listProcessEnviron :: T.Text -> IO [(T.Text, T.Text)]
listProcessEnviron process = do
  environ <- readFile $ Internal.processEnviron process
  return $ formatEnviron $ T.pack environ

-- show the current working directory of a given process
seeCwd :: T.Text -> IO T.Text
seeCwd process = do
  (out, _) <- P.readProcess_ $ P.shell $ "readlink " ++ (Internal.processCwd process)
  return $ T.strip . TE.decodeUtf8 . C8.toStrict $ out

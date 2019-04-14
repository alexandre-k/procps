{-# LANGUAGE OverloadedStrings #-}

module Process.Internal.Linux where

import qualified Data.ByteString.Lazy.Char8 as C8
import Data.Char
import Data.List
import qualified Data.String as S
import Data.String.Utils
import System.Directory
import System.Exit
import System.FilePath.Posix
import System.IO
import qualified System.Process.Typed as P


linuxProcessesDir :: String
linuxProcessesDir = "/proc"

linuxProcessName :: String
linuxProcessName = "comm"

linuxProcessCommand :: String
linuxProcessCommand = "cmdline"

linuxProcessEnviron :: String
linuxProcessEnviron = "environ"

linuxProcessCwd :: String
linuxProcessCwd = "cwd"

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

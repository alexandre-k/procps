{-# LANGUAGE CPP #-}
module Process.Internal.Common where

import Data.Char
#if linux_HOST_OS
import qualified Process.Internal.Linux as Linux


processesDir :: String
processesDir = Linux.processesDir

processName :: String
processName = Linux.processName

processCommand :: String
processCommand = Linux.processName

processEnviron :: String
processEnviron = Linux.processEnviron

processCwd :: String
processCwd = Linux.processCwd
#else
import qualified Process.Internal.BSD as BSD


processesDir :: String
processesDir = BSD.processesDir

processName :: String
processName = BSD.processName

processCommand :: String
processCommand = BSD.processName

processEnviron :: String
processEnviron = BSD.processEnviron

processCwd :: String
processCwd = BSD.processCwd
#endif

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

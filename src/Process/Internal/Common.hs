module Process.Internal.Common where

import Data.Char
import System.Info
import qualified Process.Internal.Linux as Linux
import qualified Process.Internal.BSD as BSD

isLinux :: String
isLinux = os == "linux"

processesDir :: String
processesDir = if isLinux then Linux.processesDir else BSD.processesDir

processName :: String
processName = if isLinux then Linux.processName else BSD.processName

processCommand :: String
processCommand = if isLinux then Linux.processName else BSD.processName

processEnviron :: String
processEnviron = if isLinux then Linux.processEnviron else BSD.processEnviron

processCwd :: String
processCwd = if isLinux then Linux.processCwd else BSD.processCwd

-- helper function to filter processes in /proc
isInteger :: FilePath -> Bool
isInteger xs = all isDigit xs

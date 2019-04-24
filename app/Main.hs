module Main where

import qualified Process.Information as PI
import qualified Process.Manage as PM
import qualified Process.Resources as R
import qualified Data.Yaml as Y

processStore :: ByteString
processStore = [r|
process:
  - pname: "Firefox"
  - pid: 4675
  - command: "/usr/bin/firefox"
started: true
stopped: false
memoryUsage: 0
uptime: 10
status: "Online"
logFile: "./processes.log"
|]


main = do
  -- P.start "kate"
  config <- Y.decodeThrow
  print (config :: Config)
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

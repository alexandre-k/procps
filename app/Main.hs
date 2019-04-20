module Main where

import qualified Process.Internal.Common as IC
import qualified Process.Information as PI
import qualified Process.Manage as PM

main = do
  -- P.start "kate"
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

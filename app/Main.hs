module Main where

import Control.Concurrent
import qualified ProcPS as P

main = do
  -- P.start "kate"
  kate <- P.findProcess P.PName "thunderbird"
  putStrLn $ show kate
  environ <- P.processEnviron "3654"
  putStrLn $ show environ
  cwd <- P.seeCwd "3654"
  putStrLn cwd

  -- result <- P.kill $ kate !! 0
  -- putStrLn $ show result
  -- p <- processes
  -- print p

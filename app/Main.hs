module Main where

import Control.Concurrent
import qualified ProcPS as P

main = do
  P.start "kate"
  kate <- P.findProcess P.PName "kate"
  putStrLn $ show kate
  result <- P.kill $ kate !! 0
  putStrLn $ show result
  -- p <- processes
  -- print p

module Main where

import qualified ProcPS as P

main = do
  kate <- P.findProcess P.PName "kate"
  putStrLn $ show kate
  result <- P.kill $ kate !! 0
  putStrLn $ show result
  -- p <- processes
  -- print p

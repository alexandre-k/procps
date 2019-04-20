{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Process.Resources
  ( cpuUsage
  )
where

import qualified Process.Internal.Common as Internal


data CPU = CPU { usage      :: Int
               , nice       :: Int
               , system     :: Int
               , idle       :: Int
               , iowait     :: Int
               , irq        :: Int
               , softirq    :: Int
               , steal      :: Int
               , guest      :: Int
               , guest_nice :: Int
               } deriving (Show, Read)


cpuUsage :: IO CPU
cpuUsage = do
  contents <- readFile $ Internal.cpuUsage
  return $ (toCPU . readCpuUsage $ contents)
  where
    readCpuUsage :: String -> [Int]
    readCpuUsage d =
      map (\i -> read i :: Int) (cpuData d)
      where
        cpuData :: String -> [String]
        cpuData c = drop 1 . words . head . lines $ c

    -- toCPU :: String -> CPU
    -- toCPU l = read l :: CPU

    toCPU :: [Int] -> CPU
    toCPU [a, b, c, d, e, f, g, h, i, j] = CPU {
      usage=a, nice=b, system=c, idle=d, iowait=e, irq=f, softirq=g, steal=h, guest=i, guest_nice=j }

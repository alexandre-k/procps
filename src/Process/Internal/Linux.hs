module Process.Internal.Linux where


processesDir :: String
processesDir = "/proc"

processName :: String
processName = "comm"

processCommand :: String
processCommand = "cmdline"

processEnviron :: String
processEnviron = "environ"

processCwd :: String
processCwd = "cwd"

cpuInfo :: String
cpuInfo = "cpuinfo"

cpuUsage :: String
cpuUsage = "stat"

loadAvg :: String
loadAvg = "loadavg"

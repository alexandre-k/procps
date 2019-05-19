{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.CLI
  ( parse
  , parseOptions
  , withInfo
  )
where

import Data.Semigroup ((<>))
import Options.Applicative
import Process.Manage hiding (command)
import Process.Monitor
import Process.Web.Server


data Server = Server
  { ip :: String
  , port :: Int
  } deriving (Show)


data Options
  = Serve String Int
  | Start String
  | Find String
  | ListAll
  | Show String


showParser :: Parser Options
showParser = Show <$> strArgument
      ( metavar "process name"
      <> help "Show a monitored process given its unique identifier")

listAllParser :: Parser Options
listAllParser = pure ListAll

findParser :: Parser Options
findParser = Find <$> strArgument
  ( metavar "name of a process"
   <> help "Find a process given its name")

startParser :: Parser Options
startParser = Start <$> strArgument
  ( metavar "process name"
  <> help "Name of a process existing in /usr/bin")

serveParser :: Parser Options
serveParser = Serve
  <$> strOption
      ( long "ip"
      <> short 'i'
      <> metavar "x.x.x.x"
      <> value "127.0.0.1"
      <> help "IP address used for serving files"
      )
  <*> option auto
      ( long "port"
      <> short 'p'
      <> metavar "int"
      <> value 3000
      <> help "Port used to listen to connections"
      )

parseOptions :: Parser Options
parseOptions = subparser $
  command "serve"
   (withInfo serveParser "Start a web server to visualize processes through a web interface")
  <> command "start"
   (withInfo startParser "Start a process given its name")
  <> command "list"
   (withInfo listAllParser "List all started processes")
  <> command "find"
   (withInfo findParser "Find a process given its name")
  <> command "show"
   (withInfo showParser "Show a process given its name")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

parse :: Options -> IO ()
parse command =
  case command of
    Serve ip port -> putStrLn $ "Launch server: " ++ ip ++ ":" ++ show port
    Start name -> putStrLn $ "Start process " ++ name
    ListAll -> putStrLn "List all"
    Show name -> putStrLn $ "show " ++ name
    Find name -> putStrLn $ "find " ++ name

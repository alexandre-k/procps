{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Process.CLI
  ( parse
  , parseOptions
  , withInfo
  )
where

import Data.Semigroup ((<>))
import qualified Data.Text as T
import Options.Applicative
import qualified Process.Manage as MA
import qualified Process.Monitor as MO
import Process.Web.Server
import Text.PrettyPrint.Boxes (printBox, text, render, (<+>))


data Server = Server
  { ip :: String
  , port :: Int
  } deriving (Show)


data Options
  = Serve String Int
  | Start String
  | ListAll
  | Show String


showParser :: Parser Options
showParser = Show <$> strArgument
      ( metavar "process name"
      <> help "Show a monitored process given its unique identifier")

listAllParser :: Parser Options
listAllParser = pure ListAll

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
  <> command "show"
   (withInfo showParser "Show a process given its name")

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

prettyPrintM :: [MO.MonitoredProcess] -> IO ()
prettyPrintM mprocesses = mapM_ (\m -> printBox $ text (show $ MO.name m)
                               <+> text (show $ MO.started m)
                               <+> text (show $ MA.pid (MO.process m))) mprocesses

prettyPrint :: [MA.Process] -> IO ()
prettyPrint processes = mapM_ (\p -> printBox $ text (show $ MA.pname p)
                               <+> text (show $ MA.command p)
                               <+> text (show $ MA.pid p)) processes

parse :: Options -> IO ()
parse command =
  case command of
    Serve ip port -> putStrLn $ "Launch server: " ++ ip ++ ":" ++ show port
    Start name -> MA.start (T.pack name)
    ListAll -> do
      mprocesses <- MO.listAll
      case mprocesses of
        Just mprocesses ->  prettyPrintM mprocesses
        Nothing -> putStrLn "No processes found."
    Show name -> do
      mprocesses <- MA.findProcess MA.PName (T.pack name)
      prettyPrint mprocesses

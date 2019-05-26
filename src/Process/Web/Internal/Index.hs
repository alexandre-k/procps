{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Internal.Index where

import Prelude hiding (div, head, id, span)
import Control.Monad (forM_)
import qualified Data.Text as T
import Text.Blaze (Markup, toMarkup)
import Text.Blaze.Html5 (
  (!),
  Html,
  a,
  body,
  div,
  docTypeHtml,
  footer,
  h1,
  h5,
  head,
  i,
  li,
  link,
  nav,
  p,
  script,
  span,
  title,
  ul)
import Text.Blaze.Html5.Attributes (
  class_,
  href,
  id,
  rel,
  src)
import Process.Monitor (MonitoredProcess(..), listAll)
import Process.Manage (Process(..))
-- import Text.Blaze.Html.Renderer.Text as HRT
-- import Text.Blaze.Html.Renderer.String as HRS
-- import Text.Blaze.Internal as BI

index :: [MonitoredProcess] -> Html
index processes = do
  docTypeHtml $ do
    head $ do
      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Roboto:300,400,500"
      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
      link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"
      link ! rel "stylesheet" ! href "https://use.fontawesome.com/releases/v5.8.1/css/all.css"
      script ! src "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js" $ mempty
      title "ProcPS dashboard"
    body $ do
      page processes


page :: [MonitoredProcess] -> Html
page processes = do
  div ! class_ "container" $ do
    header'
    dashboard processes
    footer'


dashboard :: [MonitoredProcess] -> Html
dashboard processes = do
  p $ "dashboard"
  -- div ! class_ "row" $ do
  --   forM_ processes card


-- card :: MonitoredProcess -> Html
-- card mproc = do
--   div ! class_ "col s6 m6" $ do
--     div ! class_ "card blue-grey darken-1" $ do
--       div ! class_ "card-content white-text" $ do
--         span ! class_ "card-title" $ toMarkup (name mproc)
--         paragraph $ "PID: " ++ (pid (process mproc))
--         paragraph $ "Current state: " ++ (status mproc)
--         paragraph $"Log file: " ++ (logFile mproc)
--         paragraph $ "Memory usage: " ++ show (memoryUsage mproc)
--         paragraph $ "Uptime: " ++ show (uptime mproc)
--       div ! class_ "card-action" $ do
--         processState (status mproc)
--       where
--       processState :: String -> Html
--       processState "stopped" = do
--         a ! href "#" $ "start"
--       processState _ = do
--         a ! href "#" $ "restart"
--         a ! href "#" $ "stop"
--       paragraph :: T.Text -> Markup
--       paragraph txt = p $ toMarkup txt


header' :: Html
header' = do
  nav ! class_ "nav-wrapper" $ do
    a ! class_ "brand-logo center" ! href "/" $ "ProcPS Dashboard"

footer' :: Html
footer' = do
  footer ! class_ "page-footer" $ do
    div ! class_ "container" $ do
      div ! class_ "row" $ do
        div ! class_ "col 12 s12" $ do
          h5 ! class_ "white-text" $ "ProcPS"
          p ! class_ "grey-text text-lighten-4" $ "Any issue should be reported here:"
          a ! href "https://github.com/alexandre-k/procps/issues" $ "-> issues on the Github page"
    div ! class_ "footer-copyright" $ do
      div ! class_ "container" $ do
        i ! class_ "far fa-copyright" $ "Licensed under BSD 3-Clause"

{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Internal.Index where

import Prelude hiding (div, head, id, span)
import Text.Blaze.Html5 (
  (!),
  Html,
  a,
  body,
  div,
  docTypeHtml,
  footer,
  h1,
  h3,
  head,
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
-- import Text.Blaze.Html.Renderer.Text as HRT
-- import Text.Blaze.Html.Renderer.String as HRS
-- import Text.Blaze.Internal as BI

index :: Html
index = do
  docTypeHtml $ do
    head $ do
      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/css?family=Roboto:300,400,500"
      link ! rel "stylesheet" ! href "https://fonts.googleapis.com/icon?family=Material+Icons"
      link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/css/materialize.min.css"
      script ! src "https://cdnjs.cloudflare.com/ajax/libs/materialize/1.0.0/js/materialize.min.js" $ mempty
      title "ProcPS dashboard"
    body $ do
      page


page :: Html
page = do
  div ! class_ "row" $ do
    div ! class_ "col s12 m12" $ do
        header'
        dashboard
        footer'


dashboard :: Html
dashboard = do
  div ! class_ "row" $ do
    div ! class_ "col s6 m6" $ do
      div ! class_ "card blue-grey darken-1" $ do
        div ! class_ "card-content white-text" $ do
          span ! class_ "card-title" $ "Firefox"
          p "Process Information"
        div ! class_ "card-action" $ do
          a ! href "#" $ "restart process"
          a ! href "#" $ "stop process"


header' :: Html
header' = do
  nav $ do
    div ! class_ "brand-logo" $ "ProcPS Dashboard"
    -- ul ! id "nav-mobile" ! class_ "right hide-on-med-and-down" $ do
    --   li $ a ! href "#" $ "Home"


footer' :: Html
footer' = do
  footer ! class_ "page-footer" $ do
    div ! class_ "container" $ do
      div ! class_ "row" $ do
        div ! class_ "col 16 s12" $ do
          h3 ! class_ "white-text" $ "footer"

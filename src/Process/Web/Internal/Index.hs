{-# LANGUAGE OverloadedStrings #-}
module Process.Web.Internal.Index where

import Text.Blaze as TB
import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as HA
import Text.Blaze.Html.Renderer.Text as HRT
import Text.Blaze.Html.Renderer.String as HRS
import Text.Blaze.Internal as BI

index :: H.Html
index = do
  H.docTypeHtml $ do
    H.head $ do
      H.title "ProcPS dashboard"

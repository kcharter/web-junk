module Widget.PageHeader where

import Import

pageHeader :: Text -> Widget
pageHeader headerText = $(widgetFile "page-header")

buildPageHeaderI :: RenderMessage HomeBase message => message -> Handler Widget
buildPageHeaderI msg = do
  render <- getMessageRender
  return $ pageHeader $ render msg

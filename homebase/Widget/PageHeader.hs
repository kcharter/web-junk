module Widget.PageHeader where

-- We could just import 'Import' here and use the 'Widget' and
-- 'Handler' types, but I'd like to be able to use this widget in the
-- login handler, which is defined in the 'Foundation' module along
-- with the app data type ('Import' re-exports 'Foundation)', so we'd
-- have a circular reference. Further, 'Widget' and 'Handler' depend
-- on the app data type. At the cost of more general types and having
-- to make more imports, we can avoid importing the 'Foundation'
-- module here and thus break the circular reference.

import Control.Monad
import Data.Text (Text)
import Prelude (($))

import Yesod.Handler (GHandler, getMessageRender)
import Yesod.Message
import Yesod.Widget (GWidget)

import Settings (widgetFile)

pageHeader :: Text -> GWidget sub master ()
pageHeader headerText = $(widgetFile "page-header")

buildPageHeaderI :: RenderMessage master message => message -> GHandler sub master (GWidget sub master ())
buildPageHeaderI msg = do
  render <- getMessageRender
  return $ pageHeader $ render msg

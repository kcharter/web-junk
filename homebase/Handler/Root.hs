module Handler.Root where

import Control.Monad (join)

import Import
import Yesod.Auth

import Widget.PageHeader

-- This is a handler function for the GET request method on the RootR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getRootR :: Handler RepHtml
getRootR = do
  maid <- maybeAuthId
  muserName <- maybe (return Nothing) getUserName maid
  header <- buildPageHeaderI MsgHomeBase
  defaultLayout $ do
    h2id <- lift newIdent
    setTitle "homebase homepage"
    header
    $(widgetFile "homepage")
  where getUserName userId = runDB $ do
          maybeUser <- get userId
          return $ join $ fmap userFullName maybeUser

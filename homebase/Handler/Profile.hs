module Handler.Profile where

import qualified Data.Text as T

import Import
import Yesod.Auth

import Handler.Root

getProfileR :: Handler RepHtml
getProfileR = do
  aid <- requireAuthId
  user <- runDB $ get404 aid
  ((_, widget), enctype) <- generateFormPost (profileForm user)
  defaultLayout $ do
    setTitle "homebase profile"
    $(widgetFile "profile")

postProfileR :: Handler RepHtml
postProfileR = do
  aid <- requireAuthId
  user <- runDB $ get404 aid
  ((result, widget), enctype) <- runFormPost (profileForm user)
  master <- getYesod
  langs <- fmap reqLangs getRequest
  let content = $(widgetFile "profile")
  case result of
    FormSuccess updatedUser ->
      do runDB $ update aid [UserFullName =. (userFullName updatedUser)]
         setMessage $ toHtml $ renderMessage master langs MsgSuccess
         getRootR
    FormMissing ->
      do setMessage $ toHtml $ renderMessage master langs MsgMissingFormData
         defaultLayout content
    FormFailure msgs ->
      do setMessage $ toHtml $ "There are validation errors: " ++ show msgs
         defaultLayout content

-- Unlike the forms in the examples in the 'Persistent' chapter of the
-- Yesod book, we want to expose only part of a 'User' in the form. It
-- seems most natural to have the form transform a given user into a
-- new one. I suppose that this potentially exposes the 'id' field to
-- change, but that's better than having to add more data types that
-- represent the 'guts' of a 'User'.

profileForm :: User -> Form User
profileForm user =
  renderDivs $ updateFullName <$>
  areq textField "Name" (userFullName user)
    where updateFullName name = user { userFullName = toName name }
          toName t | T.null t = Nothing
                   | otherwise = Just t

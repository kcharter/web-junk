{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Yesod

-- An example with a couple of widgets with their own JavaScript
-- functionality. I've added a 'withJQuery' function to endow a widget
-- with a reference to JQuery; I imagine a better way to do this is to
-- put the reference in the default layout function.

data WithWidget = WithWidget

mkYesod "WithWidget" [parseRoutes|
                      / RootR GET
                      /molly MollyR GET
                      /gordon GordonR GET
                      |]

instance Yesod WithWidget where
  approot _ = ""

getRootR :: Handler RepHtml
getRootR =
  defaultLayout $ withJQuery $ do
    setTitle "A gallery of friends"
    [whamlet|
<h1>A gallery of friends
<p>These are some of our friends:
<ul>
  <li>
    <a href=@{MollyR}>Molly
  <li>
    <a href=@{GordonR}>Gordon
     |]

getMollyR :: Handler RepHtml
getMollyR = friendR "Molly" "Molly is the greatest St. Bernard ever!"

getGordonR :: Handler RepHtml
getGordonR = friendR "Gordon" "Gordon has been my best friend for 45 years!"

friendR name desc = defaultLayout $ do
  setTitle $ toHtml $ name ++ "'s Page"
  friendW (T.pack name) (T.pack desc)

-- Hides the description paragraph below the name, unless the user
-- clicks the name. I go this out of the JQuery tutorial at
-- http://docs.jquery.com/Tutorials:Getting_Started_with_jQuery
friendW name desc = withJQuery $ do
  [whamlet|
<h1 id=#{name}>#{name}
<p>#{desc}
^{homeW}
  |]
  toWidget [julius|
$(function() {
    $("##{name}")
    .next()
    .hide()
    .prev()
    .click(function(){ $(this).next().slideToggle(); });
});
|]

homeW =
  [whamlet|<a href=@{RootR}>Back to the Gallery|]

withJQuery w =
  addScriptRemote "https://ajax.googleapis.com/ajax/libs/jquery/1.6.2/jquery.min.js"
  >> w

main :: IO ()
main = warpDebug 3000 WithWidget

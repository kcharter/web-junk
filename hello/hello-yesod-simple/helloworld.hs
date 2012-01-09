{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

-- [Kevin] The comments here aren't in the original source. I've added
-- them mostly to reiterate things I learned from the Yesod book.

import Yesod

-- This is the *foundation* type for the application. This one is
-- trivial, but a generated foundation type generally has a lot more
-- stuff in it, related to application settings.

data HelloWorld = HelloWorld

mkYesod "HelloWorld" [parseRoutes|
                      / HomeR GET
                      |]

instance Yesod HelloWorld where
  approot _ = ""

-- We've got one route above, resource "HomeR" available at "/", using
-- the "GET" method. For that route, there has to be a corresponding
-- handler function, whose name has the form <method><resource>. Here
-- it is. 'defaultLayout' here provides a simple html wrapper with.

-- Loading this into ghci, 'defaultLayout' has the type
--
-- Yesod a => GWidget sub a () -> GHandler sub a RepHtml
--
-- which explains why we've got the "whamlet" quasiquotation instead
-- of, say, a string.
getHomeR :: Handler RepHtml
getHomeR = defaultLayout [whamlet|Hello World! This is my first ever Yesod web app.|]

-- Here, we've gone directly to 'warpDebug', but a scaffolded
-- application would do this in a more WAI-ish way and thus be easily
-- retargetable at other web app environments for which there is a WAI
-- 'adapter' (for lack of a better word).
main :: IO ()
main = warpDebug 3000 HelloWorld

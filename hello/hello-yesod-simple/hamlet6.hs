{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text (Text)
import qualified Data.Text as T
import Text.Hamlet (HtmlUrlI18n, ihamlet)
import Text.Blaze (toHtml)
import Text.Blaze.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

renderUrl :: MyRoute -> [(Text,Text)] -> Text
renderUrl Home _ = "/home"
renderUrl Time _ = "/time"
renderUrl Stylesheet _ = "/style.css"

-- A type for messages that are subject to localization. It doesn't
-- look like 'mkYesod' has support for generating a type for messages,
-- in the same way that it does for generating a type for routes. I
-- imagine that will be forthcoming?

data Msg = Hello | Apples Int

-- For each language you want to support, there has to be a different
-- renderer. Or, strictly speaking, for each *locale* you want to
-- support.

renderEnglish :: Msg -> Text
renderEnglish Hello = "Hello"
renderEnglish (Apples 0) = "You did not buy any apples."
renderEnglish (Apples 1) = "You bought one apple."
renderEnglish (Apples i) = T.concat ["You bought ", T.pack $ show i, " apples."]

renderGerman :: Msg -> Text
renderGerman Hello = "Guten tag!"
renderGerman (Apples 0) = "Sie haben kleine Apfel kaufen."
renderGerman (Apples 1) = "Sie haben eine Apfel kaufen."
renderGerman (Apples i) = T.concat ["Sie haben ", T.pack $ show i, " Apfel kaufen."]

-- A template with internationalized messages uses two renderers, one
-- for messages and one for URLs. The HtmlUrlI18n type is in a fact a
-- function from a message renderer and a URL renderer to Html.

template :: Int -> HtmlUrlI18n Msg MyRoute
template count = [ihamlet|
!!!
<html>
  <head>
    <title>i18n
  <body>
    <h1>_{Hello}
    <p>_{Apples count}
|]

-- Note that this is pretty rudimentary i18n support. Yesod doesn't
-- seem to have a preference for a scheme of locale identifiers and a
-- way to organize groups of messages by locale; i.e. something like
-- locales and message bundles in Java. I'm not familiar with any
-- Haskell packages for doing I18N/L10N, so perhaps there just isn't a
-- widely-adopted standard.

main :: IO ()
main = do
  putStrLn "In English:"
  putStrLn $ renderHtml
    $ (template 5) (toHtml . renderEnglish) renderUrl
  putStrLn "In incorrect German:"
  putStrLn $ renderHtml
    $ (template 8) (toHtml . renderGerman) renderUrl

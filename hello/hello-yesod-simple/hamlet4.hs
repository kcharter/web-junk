{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- This example shows using quasiquotes for building pages (though the
-- previous examples show this too).  The hamlet5 example shows the
-- same thing but using template Haskell and external files.

import Text.Hamlet (HtmlUrl, hamlet)
import Data.Text (Text)
import Text.Blaze.Renderer.String (renderHtml)

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text,Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

template :: Text -> HtmlUrl MyRoute
template title = [hamlet|
!!!
<html>
  <head>
    <title>#{title}
    <link rel=stylesheet href=@{Stylesheet}>
  <body>
    <h1>#{title}
|]

main :: IO ()
main = putStrLn $ renderHtml $ template "My Title" render

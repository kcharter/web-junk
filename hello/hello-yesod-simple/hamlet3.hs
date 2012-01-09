{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

-- I've taken the source from hamlet2.hs and reworked it a bit.

import Text.Hamlet (HtmlUrl, hamlet)
import Text.Blaze.Renderer.String (renderHtml)
import Data.Text (Text)

-- I'm guessing that in a Yesod app, there is one type for the routes,
-- and all the data constructors are collected under that type, so we
-- can define a single rendering function.

data MyRoute = Home | Gordon | Molly

render :: MyRoute -> [(Text,Text)] -> Text
render Home _ = "/home"
render Gordon _ = "/gordon"
render Molly _ = "/molly"

footer :: HtmlUrl MyRoute
footer = [hamlet|
<footer>
  Return to #
  <a href=@{Home}>Homepage
  .
|]

home :: HtmlUrl MyRoute
home = [hamlet|
<body>
  <p>This is my page. But there are others:
  <ul>
    <li><a href=@{Gordon}>Gordon
    <li><a href=@{Molly}>Molly
  ^{footer}
|]

gordon :: HtmlUrl MyRoute
gordon = [hamlet|
<p>Gordon has been my best friend for 45 years!
<p>But you might also be interested in <a href=@{Molly}>.
^{footer}
|]

molly :: HtmlUrl MyRoute
molly = [hamlet|
<p>Molly is the world's greatest St. Bernard!
<p>She's pals with <a href=@{Gordon}> too.
^{footer}
|]


main :: IO ()
main = do
  putStrLn "The main page:"
  printHtml home
  putStrLn "Gordon's page:"
  printHtml gordon
  putStrLn "Molly's page:"
  printHtml molly
  where printHtml p = putStrLn $ renderHtml $ p render

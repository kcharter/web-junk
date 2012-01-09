{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}

import Text.Lucius (CssUrl, luciusFile, luciusFileDebug, renderCss)
import Data.Text (Text)
import qualified Data.Text.Lazy.IO as TLIO

data MyRoute = Home | Time | Stylesheet

render :: MyRoute -> [(Text,Text)] -> Text
render Home _ = "/home"
render Time _ = "/time"
render Stylesheet _ = "/style.css"

-- instead of defining the template in the code with quasiquotation,
-- we read it from a file. If I've understood correctly, the 'debug'
-- version of the reader re-reads the template each time it's
-- needed. This is slow but avoids recompiling the application when
-- only the template has changed.

-- However, the changes in a template have to be limited: you cannot
-- reference new variables, for example, without recompiling the
-- application. Michael Snoyman says in the book comments that this is
-- the reason why there is no longer a 'hamletFileDebug', since it's
-- very common to change the referenced variables when updating an
-- HTML template.

-- Note that, even when this isn't production, it's a compile-time
-- error if the file doesn't exist. That's kind of nice. For
-- production, the template becomes baked into the executable. If you
-- modify the template, the executable doesn't pick up the
-- modifications. With debug, it does.

template :: CssUrl MyRoute
#if PRODUCTION
template = $(luciusFile "template.lucius")
#else
template = $(luciusFileDebug "template.lucius")
#endif

main :: IO ()
main = do
#if PRODUCTION
  putStrLn "This is production."
#else
  putStrLn "This is *not* production."
#endif
  TLIO.putStrLn $ renderCss $ template render

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}

import Text.Shakespeare.Text
import qualified Data.Text.Lazy.IO as TLIO
import qualified Data.Text.IO as TIO
import Data.Text (Text)
import Control.Monad (forM_)

-- The 'lt' and 'st' quasiquoters let you build ordinary text by
-- interpolating values. 'lt' is for lazy text, 'st' is for
-- strict. The original example uses just 'lt', but I've added 'st'
-- just to try it out.

data Item = Item { itemName :: Text
                 , itemQty :: Int
                 }

items :: [Item]
items = [Item "apples" 5, Item "bananas" 10]

main :: IO ()
main = do
  putStrLn "Using lazy text:"
  forM_ items $ \item ->
    TLIO.putStrLn
    [lt|You have #{show $ itemQty item} #{itemName item}.|]
  putStrLn "Using strict text:"
  forM_ items $ \item ->
    TIO.putStrLn
    [st|You have #{show $ itemQty item} #{itemName item}.|]

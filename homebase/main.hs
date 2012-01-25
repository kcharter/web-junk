import Yesod.Default.Config (fromArgs)
import Yesod.Default.Main   (defaultMain)
import Application          (withHomeBase)
import Prelude              (IO)

main :: IO ()
main = defaultMain fromArgs withHomeBase
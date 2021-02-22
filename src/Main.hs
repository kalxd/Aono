import qualified GHC.IO.Encoding as E

import Aono.Option.App (runAppWith, readAppConfig)
import Aono.Route (runRoute)

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8 -- 设置编码环境，在windows上尤为重要。
    config <- readAppConfig
    runAppWith config $ runRoute config

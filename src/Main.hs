import Hakyll
import Aono.Type
import Aono.Route

main :: IO ()
main = do
    config <- loadConfig
    hakyllWith (applyHakyllConfig config) $ runRoute config

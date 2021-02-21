import Aono.Option.App (runAppWith, readAppConfig)
import Aono.Route (runRoute)

main :: IO ()
main = do
    config <- readAppConfig
    runAppWith config $ runRoute config

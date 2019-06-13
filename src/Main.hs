{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import Aono.Type
import Aono.Route

main :: IO ()
main = do
    config <- loadConfig
    route <- runRoute config
    hakyllWith (applyHakyllConfig config) route

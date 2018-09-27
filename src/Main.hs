{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import XG.Type
import XG.Route

main :: IO ()
main = do
    config <- loadConfig
    route <- runRoute config
    hakyllWith (applyHakyllConfig config) route

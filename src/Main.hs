{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import XG.Type
import XG.Route

main :: IO ()
main = do
    config <- loadConfig
    hakyllWith (applyHakyllConfig config) $ do
        imageRoute
        cssRoute
        postRoute postPattern
        indexRoute
        templateRoute

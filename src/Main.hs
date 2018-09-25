{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import XG.Type
import XG.Route
import Config

main :: IO ()
main = do
    hakyllWith (combineConfig config) $ do
        imageRoute
        cssRoute
        postRoute postPattern
        indexRoute
        templateRoute

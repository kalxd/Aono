{-# LANGUAGE OverloadedStrings #-}
import Hakyll
import XG.Type
import XG.Route
import Config

main :: IO ()
main = do
    kimochi <- randomKimochi
    hakyllWith (combineConfig config) $ do
        imageRoute
        cssRoute
        postRoute postPattern
        indexRoute kimochi
        templateRoute

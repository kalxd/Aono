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
        cssRoute $ fromGlob $ "css/" <> toCSSName kimochi
        postRoute postPattern
        indexRoute
        templateRoute

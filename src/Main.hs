{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import XG.Type
import Config

postPattern :: Pattern
postPattern = fromGlob $ sitePostDir config <> "/*"

copyProcess :: Rules ()
copyProcess = route idRoute >> compile copyFileCompiler

imageRoute :: Rules ()
imageRoute = match "image/**" copyProcess

cssRoute :: Pattern -> Rules()
cssRoute pat = match pat copyProcess

postRoute :: Pattern -> Rules ()
postRoute pat = match pat $ do
    route $ setExtension "html"
    compile $ do
        pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html" postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

indexRoute :: Rules ()
indexRoute = match "index.html" $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let ctx = listField "posts" postCtx (return posts)
                  <> constField "title" "首页"
                  <> defaultContext
        getResourceBody
            >>= applyAsTemplate ctx
            >>= loadAndApplyTemplate "templates/default.html" ctx
            >>= relativizeUrls

templateRoute :: Rules ()
templateRoute = match "templates/*" $ compile templateBodyCompiler

main :: IO ()
main = do
    kimochi <- randomKimochi
    hakyllWith (combineConfig config) $ do
        imageRoute
        cssRoute $ fromGlob $ "css/" <> toCSSName kimochi
        postRoute postPattern
        indexRoute
        templateRoute


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

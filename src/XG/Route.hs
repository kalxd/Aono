{-# LANGUAGE OverloadedStrings #-}
module XG.Route where

import Hakyll

import Data.Monoid ((<>))
import Control.Monad ((>=>))

import XG.Type
import Config

postPattern :: Pattern
postPattern = fromGlob $ sitePostDir config <> "/**.org"

copyProcess :: Rules ()
copyProcess = route idRoute >> compile copyFileCompiler

imageRoute :: Rules ()
imageRoute = match "image/**" copyProcess

cssRoute :: Pattern -> Rules()
cssRoute pat = match pat $ do
    route idRoute
    compile compressCssCompiler

postRoute :: Pattern -> Rules ()
postRoute pat = match pat $ do
    route $ setExtension "html"
    compile $ do
        pandocCompiler
            >>= loadAndApplyTemplate "tpl/wfvh.html" defaultContext
            >>= applyLayout defaultContext

indexRoute :: Rules ()
indexRoute = create ["index.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let ctx = listField "posts" postCtx (return posts)
                  <> constField "title" "首页"
                  <> defaultContext
        makeItem ""
            >>= loadAndApplyTemplate "tpl/index.html" ctx
            >>= applyLayout ctx

templateRoute :: Rules ()
templateRoute = match "tpl/*" $ compile templateBodyCompiler

-- | 载入模板
applyLayout :: Context a -> Item a -> Compiler (Item String)
applyLayout ctx = loadAndApplyTemplate "tpl/layout.html" ctx >=> relativizeUrls

postCtx :: Context String
postCtx = dateField "date" "%B %e, %Y" <> defaultContext

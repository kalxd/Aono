{-# LANGUAGE OverloadedStrings #-}
module XG.Route where

import Hakyll
import Text.Pandoc.Options (WriterOptions(..))

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

cssRoute :: Rules()
cssRoute = match "css/*" $ do
    route idRoute
    compile compressCssCompiler

postRoute :: Pattern -> Rules ()
postRoute pat = match pat $ do
    route $ setExtension "html"
    compile $ do
        toc <- flip getMetadataField "toc" =<< getUnderlying
        let writeSet = case fmap read toc :: Maybe Int of
                Just n -> defaultHakyllWriterOptions { writerTableOfContents = True
                                                     , writerTOCDepth = n
                                                     , writerTemplate = Just "$toc$\n$body$"
                                                     }
                Nothing -> defaultHakyllWriterOptions
        let ctx = pageCtx
        pandocCompilerWith defaultHakyllReaderOptions writeSet
            >>= loadAndApplyTemplate "tpl/wfvh.html" ctx
            >>= applyLayout ctx

indexRoute :: Rules ()
indexRoute = create ["index.html"] $ do
    route idRoute
    compile $ do
        posts <- recentFirst =<< loadAll postPattern
        let ctx = mconcat [ listField "posts" pageCtx (return posts)
                          , constField "title" "首页"
                          , defaultContext
                          ]
        makeItem ""
            >>= loadAndApplyTemplate "tpl/index.html" ctx
            >>= applyLayout ctx

templateRoute :: Rules ()
templateRoute = match "tpl/*" $ compile templateBodyCompiler

-- | 载入模板
applyLayout :: Context a -> Item a -> Compiler (Item String)
applyLayout ctx = loadAndApplyTemplate "tpl/layout.html" ctx >=> relativizeUrls

-- | 附加其它信息
pageCtx :: Context String
pageCtx = mconcat [ dateField "date" "%B %e, %Y"
                  , defaultContext
                  ]

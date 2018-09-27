{-# LANGUAGE OverloadedStrings #-}
module XG.Route where

import Hakyll
import Text.Pandoc.Options (WriterOptions(..))

import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Control.Monad.Trans.Reader
import Control.Monad ((>=>))

import XG.Type

type RouteEnv a = ReaderT SiteConfig IO a
type RouteRule = RouteEnv (Rules ())

routeRule :: RouteRule
routeRule = do
    postDir <- asks sitePostDir
    title <- asks siteTitle

    let postPattern = fromGlob $ postDir <> "/**"
    return $ do
        -- image route
        match "image/**" $ route idRoute >> compile copyFileCompiler
        -- css route
        match "css/*" $ route idRoute >> compile compressCssCompiler
        -- post route
        match postPattern $ do
            route $ setExtension "html"
            compile $ do
                toc <- flip getMetadataField "toc" =<< getUnderlying
                let writeSet = case toc >>= readMaybe :: Maybe Int of
                        Just n -> defaultHakyllWriterOptions { writerTableOfContents = True
                                                             , writerTOCDepth = n
                                                             , writerTemplate = Just "$toc$\n$body$"
                                                             }
                        Nothing -> defaultHakyllWriterOptions
                let ctx = pageCtx
                pandocCompilerWith defaultHakyllReaderOptions writeSet
                    >>= loadAndApplyTemplate "tpl/wfvh.html" ctx
                    >>= applyLayout ctx
        -- index route
        create ["index.html"] $ do
            route idRoute
            compile $ do
                postAry <- recentFirst =<< loadAll postPattern
                let ctx = mconcat [ listField "postAry" pageCtx (return postAry)
                                  , constField "title" "首页"
                                  , defaultContext
                                  ]
                makeItem ""
                    >>= loadAndApplyTemplate "tpl/index.html" ctx
                    >>= applyLayout ctx
        -- template
        match "tpl/*" $ compile templateCompiler


runRoute :: SiteConfig -> IO (Rules ())
runRoute = runReaderT routeRule

-- | 载入模板
applyLayout :: Context a -> Item a -> Compiler (Item String)
applyLayout ctx = loadAndApplyTemplate "tpl/layout.html" ctx >=> relativizeUrls

-- | 附加其它信息
pageCtx :: Context String
pageCtx = mconcat [ dateField "date" "%B %e, %Y"
                  , defaultContext
                  ]

{-# LANGUAGE OverloadedStrings #-}
module XG.Route where

import Hakyll
import Text.Pandoc.Options (WriterOptions(..))
import System.Directory (listDirectory, doesDirectoryExist)
import System.FilePath.Posix ((</>))

import Text.Read (readMaybe)
import Data.Monoid ((<>))
import Data.List (sort)
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Class (lift)
import Control.Monad ((>=>), filterM)

import XG.Type

type RouteEnv a = ReaderT SiteConfig IO a
type RouteRule = RouteEnv (Rules ())

-- | 路径转化成列表
toListItem :: [String] -> Compiler [Item String]
toListItem xs = return $ do
    x <- xs
    let p = fromFilePath x
    return $ Item p x

-- | 读取菜单路径
readMenu :: RouteEnv [String]
readMenu = do
    dirPath <- asks sitePostDir
    dirs <- lift $ listDirectory dirPath
    filterM (lift . doesDirectoryExist . (dirPath </>)) dirs

-- | 模板需要用到的全部变量都在这里
globalCtx :: RouteEnv (Context String)
globalCtx = do
    config <- ask
    dirs <- readMenu

    return $ mconcat [ constField "title" $ siteTitle config
                     , listField "menu" defaultContext $ toListItem dirs
                     , defaultContext
                     ]

routeRule :: RouteRule
routeRule = do
    postDir <- asks sitePostDir
    title <- asks siteTitle
    gctx <- globalCtx

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
                let ctx = pageCtx <> gctx
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
                                  , gctx
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

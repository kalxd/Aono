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
    dirs <- lift $ sort <$> listDirectory dirPath
    filterM (lift . doesDirectoryExist . (dirPath </>)) dirs

-- | 空元素
emptyItem :: Compiler (Item String)
emptyItem = makeItem ""

-- | 模板需要用到的全部变量都在这里
globalCtx :: RouteEnv (Context String)
globalCtx = do
    config <- ask
    dirs <- readMenu

    return $ mconcat [ constField "siteTitle" $ siteTitle config
                     , listField "menu" defaultContext $ toListItem dirs
                     , defaultContext
                     ]

-- | 载入模板
applyLayout :: Context a -> Item a -> Compiler (Item String)
applyLayout ctx = loadAndApplyTemplate "tpl/layout.html" ctx >=> relativizeUrls

-- | 渲染出最终模板
renderTpl :: Identifier -> Context String -> Item String -> Compiler (Item String)
renderTpl tpl ctx = loadAndApplyTemplate tpl ctx >=> applyLayout ctx

-- | 从空模板开始渲染
renderFromEmpty :: Identifier -> Context String -> Compiler (Item String)
renderFromEmpty tpl ctx = emptyItem >>= renderTpl tpl ctx

-- | 附加其它信息
pageCtx :: Context String
pageCtx = mconcat [ dateField "date" "%B %e, %Y"
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

        -- 猫类
        cats <- buildCategories postPattern $ fromCapture "猫/*.html"
        tagsRules cats $ \cat p -> do
            route idRoute
            compile $ do
                postAry <- recentFirst =<< loadAll p
                let ctx = mconcat [ listField "postAry" pageCtx $ return postAry
                                  , constField "title" $ "分类：" <> cat
                                  , gctx
                                  ]
                renderFromEmpty "tpl/cat.html" ctx

        -- post route
        match postPattern $ do
            route $ setExtension "html"
            compile $ do
                toc <- flip getMetadataField "toc" =<< getUnderlying
                let tocTpl = "<div class=\"toc\">目录：$toc$</div>$body$"
                let writeSet = case toc >>= readMaybe :: Maybe Int of
                        Just n -> defaultHakyllWriterOptions { writerTableOfContents = True
                                                             , writerTOCDepth = n
                                                             , writerTemplate = Just tocTpl
                                                             }
                        Nothing -> defaultHakyllWriterOptions
                let ctx = categoryField "cats" cats <> gctx
                pandocCompilerWith defaultHakyllReaderOptions writeSet
                    >>= renderTpl "tpl/wfvh.html" ctx

        -- index route
        create ["index.html"] $ do
            route idRoute
            compile $ do
                postAry <- recentFirst =<< loadAll postPattern
                let ctx = mconcat [ listField "postAry" pageCtx (return postAry)
                                  , constField "title" "首页"
                                  , gctx
                                  ]
                renderFromEmpty "tpl/index.html" ctx

        -- rss
        create ["rss.xml"] $ do
            route idRoute
            compile $ do
                let config = FeedConfiguration title "" "" "" "http://abc.com"
                let ctx = mconcat [ constField "description" ""
                                  , gctx
                                  ]
                posts <- fmap (take 10) . recentFirst =<< loadAll postPattern
                renderRss config ctx posts

        -- template
        match "tpl/*" $ compile templateCompiler


runRoute :: SiteConfig -> IO (Rules ())
runRoute = runReaderT routeRule

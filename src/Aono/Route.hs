{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Aono.Route where

import Hakyll
import Text.Pandoc.Options (WriterOptions(..))

import Text.Read (readMaybe)
import Control.Monad ((>=>))

import Aono.Type

-- | 空元素
emptyItem :: Compiler (Item String)
emptyItem = makeItem ""

-- | 模板需要用到的全部变量都在这里
globalCtx :: SiteConfig -> (Context String)
globalCtx SiteConfig{..} = mconcat [ constField "siteTitle" siteTitle
                                   , maybe mempty (constField "siteDesc") $ siteDesc
                                   , defaultContext
                                   ]

-- | 载入模板
applyLayout :: Context a -> Item a -> Compiler (Item String)
applyLayout ctx = loadAndApplyTemplate "tpl/layout.html" ctx >=> relativizeUrls

-- | 渲染出最终模板
renderTpl :: Identifier -> Context String -> Item String -> Compiler (Item String)
renderTpl tpl ctx = loadAndApplyTemplate tpl ctx >=> saveSnapshot "content" >=> applyLayout ctx

-- | 从空模板开始渲染
renderFromEmpty :: Identifier -> Context String -> Compiler (Item String)
renderFromEmpty tpl ctx = emptyItem >>= renderTpl tpl ctx

-- | 附加其它信息
pageCtx :: Context String
pageCtx = mconcat [ dateField "date" "%Y年%m月%d日"
                  , dateField "datetime" "%Y-%m-%d"
                  , defaultContext
                  ]

-- | 如何分页
pageGroup :: (MonadMetadata m, MonadFail m) => Int -> [Identifier] -> m [[Identifier]]
pageGroup pageSize = fmap (paginateEvery pageSize) . sortRecentFirst

-- | 分页地址
pageLink :: PageNumber -> Identifier
pageLink n = fromFilePath $ "page/" <> show n <> "/index.html"

-- 标签云渲染方式
renderCloud :: Tags -> Compiler String
renderCloud = renderTagCloud 100 500

runRoute :: SiteConfig -> Rules ()
runRoute config@SiteConfig{..} = do
    let gctx =  globalCtx config
        postPattern = fromGlob $ sitePostDir <> "/**"

    -- image route
    match "image/**" $ route idRoute >> compile copyFileCompiler

    -- css route
    match "css/*" $ route idRoute >> compile compressCssCompiler

    -- 标签分类
    tags <- buildTags postPattern $ fromCapture "标签/*.html"
    -- 猫类
    cats <- buildCategories postPattern $ fromCapture "猫/*.html"

    -- 生成标签页
    tagsRules tags $ \tag p -> do
        route idRoute
        compile $ do
            postAry <- recentFirst =<< loadAll p
            let ctx = mconcat [ listField "postAry" pageCtx $ return postAry
                              , constField "title" $ "标签：" <> tag
                              , gctx
                              ]
            renderFromEmpty "tpl/bnqm.html" ctx

    -- 生成标签页
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
            -- let tocTpl = "<div class=\"toc\">目录：$toc$</div>$body$"
            let writeSet = case toc >>= readMaybe :: Maybe Int of
                    Just n -> defaultHakyllWriterOptions { writerTableOfContents = True
                                                         , writerTOCDepth = n
                                                         -- , writerTemplate = Just tocTpl
                                                         , writerTemplate = Nothing
                                                         }
                    Nothing -> defaultHakyllWriterOptions
            let ctx = tagsField "tags" tags <> pageCtx <> gctx
            pandocCompilerWith defaultHakyllReaderOptions writeSet
                >>= renderTpl "tpl/wfvh.html" ctx

    -- 分页
    page <- buildPaginateWith (pageGroup sitePageSize) postPattern pageLink

    -- 所有猫
    create ["猫.html"] $ do
        route idRoute
        compile $ do
            catCloud <- renderCloud cats
            let ctx = mconcat [ constField "html" catCloud
                              , constField "title" "猫云"
                              , gctx
                              ]
            renderFromEmpty "tpl/yy.html" ctx

    -- 所有标签
    create ["标签.html"] $ do
        route idRoute
        compile $ do
            tagCloud <- renderCloud tags
            let ctx = mconcat [ constField "html" tagCloud
                              , constField "title" "标签云"
                              , gctx
                              ]
            renderFromEmpty "tpl/yy.html" ctx

    -- 首页
    create ["index.html"] $ do
        route idRoute
        compile $ do
            postAry <- fmap (take sitePageSize) $ recentFirst =<< loadAll postPattern
            let ctx = mconcat [ listField "postAry" pageCtx (return postAry)
                              , constField "title" siteTitle
                              , paginateContext page 1
                              , gctx
                              ]
            renderFromEmpty "tpl/index.html" ctx

    paginateRules page $ \pageNum pat -> do
        route idRoute
        compile $ do
            postAry <- recentFirst =<< loadAll pat
            let title = siteTitle <> " - " <> "第" <> show pageNum <> "页"
            let ctx = mconcat [ listField "postAry" pageCtx $ pure postAry
                              , constField "title" title
                              , paginateContext page pageNum
                              , gctx
                              ]
            renderFromEmpty "tpl/index.html" ctx

    -- rss
    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let ctx = gctx <> bodyField "description"
            posts <- fmap (take sitePageSize) . recentFirst =<< loadAll postPattern
            renderAtom (feedConfig config) ctx posts

    -- template
    match "tpl/*" $ compile templateCompiler

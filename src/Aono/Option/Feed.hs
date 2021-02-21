-- | 网站rss订阅的基本信息。
{-# LANGUAGE RecordWildCards #-}
module Aono.Option.Feed (buildFeedFrom) where

import Hakyll.Web.Feed (FeedConfiguration(..))
import Data.Maybe (fromMaybe)

import Aono.Option.App (AppConfig(..))

buildFeedFrom :: AppConfig -> FeedConfiguration
buildFeedFrom AppConfig{..} =
    FeedConfiguration { feedTitle = siteTitle
                      , feedDescription = fromMaybe siteTitle siteDesc
                      , feedAuthorName = siteTitle
                      , feedAuthorEmail = ""
                      , feedRoot = siteHost
                      }

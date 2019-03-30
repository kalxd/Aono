{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module XG.Type where

import Hakyll.Core.Configuration
import Hakyll.Core.Rules
import Hakyll.Web.Feed
import Data.Yaml
import Data.Maybe

data SiteConfig = SiteConfig { siteTitle :: String -- 网站标题
                             , siteDesc :: Maybe String -- 网站描述
                             , siteHost :: String -- 开发地址
                             , sitePort :: Int -- 开发端口
                             , siteSource :: Maybe String -- 网站源码地址
                             , sitePostDir :: FilePath -- 文章目录
                             } deriving (Show)

defSiteConfig :: SiteConfig
defSiteConfig = SiteConfig { siteTitle = "我的网站"
                           , siteDesc = Nothing
                           , siteHost = previewHost defaultConfiguration
                           , sitePort = previewPort defaultConfiguration
                           , siteSource = Nothing
                           , sitePostDir = "posts"
                           }

instance FromJSON SiteConfig where
    parseJSON = withObject "config" $ \v -> SiteConfig
                                            <$> v .:? "title" .!= siteTitle defSiteConfig
                                            <*> v .:? "desc"
                                            <*> v .:? "host" .!= siteHost defSiteConfig
                                            <*> v .:? "port" .!= sitePort defSiteConfig
                                            <*> v .:? "source"
                                            <*> v .:? "postdir" .!= sitePostDir defSiteConfig

loadConfig :: IO SiteConfig
loadConfig = do
    c <- decodeFileEither "config.yml"
    case c of
        Left _ -> return defSiteConfig
        Right a -> return a

applyHakyllConfig :: SiteConfig -> Configuration
applyHakyllConfig config = defaultConfiguration { previewHost = host
                                                , previewPort = port
                                                }
    where host = siteHost config
          port = sitePort config

feedConfig :: SiteConfig -> FeedConfiguration
feedConfig SiteConfig{..} = FeedConfiguration { feedTitle = siteTitle
                                              , feedDescription = fromMaybe siteTitle siteDesc
                                              , feedAuthorName = siteTitle
                                              , feedAuthorEmail = ""
                                              , feedRoot = siteHost
                                              }

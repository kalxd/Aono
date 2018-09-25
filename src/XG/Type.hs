{-# LANGUAGE OverloadedStrings #-}
module XG.Type where

import Hakyll.Core.Configuration
import Data.Yaml
import Data.Maybe

data SiteConfig = SiteConfig { siteTitle :: String -- 网站标题
                             , siteHost :: String -- 开发地址
                             , sitePort :: Int -- 开发端口
                             , siteSource :: Maybe String -- 网站源码地址
                             , sitePostDir :: FilePath -- 文章目录
                             } deriving (Show)

instance FromJSON SiteConfig where
    parseJSON = withObject "config" $ \v -> SiteConfig
                                            <$> v .:? "title" .!= "我的网站"
                                            <*> v .:? "host" .!= previewHost defaultConfiguration
                                            <*> v .:? "port" .!= previewPort defaultConfiguration
                                            <*> v .:? "source"
                                            <*> v .:? "postdir" .!= "posts"

loadConfig :: IO SiteConfig
loadConfig = decodeFileThrow "config.yml"

applyHakyllConfig :: SiteConfig -> Configuration
applyHakyllConfig config = defaultConfiguration { previewHost = host
                                                , previewPort = port
                                                }
    where host = siteHost config
          port = sitePort config

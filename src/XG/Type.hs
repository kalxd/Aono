module XG.Type where

import Hakyll.Core.Configuration
import Data.Maybe

data SiteConfig = SiteConfig { siteTitle :: String -- 网站标题
                             , siteHost :: Maybe String -- 开发地址
                             , sitePort :: Maybe Int -- 开发端口
                             , siteSource :: Maybe String -- 网站源码地址
                             , sitePostDir :: FilePath -- 文章目录
                             } deriving (Show)

defSiteConfig :: SiteConfig
defSiteConfig = SiteConfig "我的网站" Nothing Nothing Nothing "posts"

combineConfig :: SiteConfig -> Configuration
combineConfig config = defaultConfiguration { previewHost = host
                                            , previewPort = port
                                            }
    where host = fromMaybe (previewHost defaultConfiguration) $ siteHost config
          port = fromMaybe (previewPort defaultConfiguration) $ sitePort config

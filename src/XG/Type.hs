module XG.Type where

import Hakyll.Core.Configuration
import System.Random
import Data.Maybe

data Kimochi = Ureshi -- 高兴
             | Hutsuu -- 普通
             | Kanashii -- 悲伤
             deriving (Enum, Show)

instance Random Kimochi where
    randomR (x, y) g = (toEnum i, g')
        where pair = (fromEnum x, fromEnum y)
              (i, g') = randomR pair g

    random g = (toEnum r, g')
        where (r, g') = randomR (0, 2) g

randomKimochi :: IO Kimochi
randomKimochi = randomIO


data SiteConfig = SiteConfig { siteTitle :: String -- 网站标题
                             , siteHost :: Maybe String -- 开发地址
                             , sitePort :: Maybe Int -- 开发端口
                             , siteSource :: Maybe String -- 网站源码地址
                             , sitePostDir :: Maybe FilePath -- 文章目录
                             } deriving (Show)

defSiteConfig :: SiteConfig
defSiteConfig = SiteConfig "我的网站" Nothing Nothing Nothing Nothing

combineConfig :: SiteConfig -> Configuration
combineConfig config = defaultConfiguration { previewHost = host
                                            , previewPort = port
                                            }
    where host = fromMaybe (previewHost defaultConfiguration) $ siteHost config
          port = fromMaybe (previewPort defaultConfiguration) $ sitePort config

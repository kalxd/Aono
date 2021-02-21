-- | 启动程序的必要流程
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Aono.Option.App ( AppConfig(..)
                       , readAppConfig
                       , runAppWith
                       ) where

import Hakyll.Main (hakyllWith)
import Hakyll.Core.Rules (Rules)
import Hakyll.Core.Configuration ( Configuration(..)
                                 , defaultConfiguration
                                 )

import Data.Yaml ( FromJSON(..)
                 , withObject
                 , (.:?)
                 , (.!=)
                 , decodeFileEither
                 )

data AppConfig = AppConfig { siteTitle :: String -- ^ 网站标题。
                             , siteDesc :: Maybe String -- ^ 网站描述。
                             , siteHost :: String -- ^ 开发地址。
                             , sitePort :: Int -- ^ 开发端口。
                             , siteSource :: Maybe String -- ^ 网站源码地址。
                             , sitePostDir :: FilePath -- ^ 文章目录。
                             , siteOutput :: FilePath -- ^ 最终页面输出目录。
                             , sitePageSize :: Int -- ^ 分页，每页多少文章。
                             } deriving Show

instance FromJSON AppConfig where
    parseJSON = withObject "config" $ \v -> AppConfig
                                            <$> v .:? "title" .!= siteTitle defSiteConfig
                                            <*> v .:? "desc"
                                            <*> v .:? "host" .!= siteHost defSiteConfig
                                            <*> v .:? "port" .!= sitePort defSiteConfig
                                            <*> v .:? "source"
                                            <*> (pure $ sitePostDir defSiteConfig)
                                            <*> v .:? "output" .!= siteOutput defSiteConfig
                                            <*> v .:? "pageSize" .!= sitePageSize defSiteConfig

-- | 程序默认配置。
defSiteConfig :: AppConfig
defSiteConfig = AppConfig { siteTitle = "我的网站"
                           , siteDesc = Nothing
                           , siteHost = previewHost defaultConfiguration
                           , sitePort = previewPort defaultConfiguration
                           , siteSource = Nothing
                           , sitePostDir = "文章"
                           , siteOutput = destinationDirectory defaultConfiguration
                           , sitePageSize = 23
                           }

-- | 从环境中读取配置信息。
readAppConfig :: IO AppConfig
readAppConfig = do
    c <- decodeFileEither "config.yml"
    case c of
        Right a -> pure a
        Left _ -> do
            putStrLn "读取配置失败，使用默认配置"
            pure defSiteConfig

-- | 根据配置运行程序
runAppWith :: AppConfig -> Rules a -> IO ()
runAppWith AppConfig{..} = hakyllWith config'
    where config' = defaultConfiguration { previewHost = siteHost
                                         , previewPort = sitePort
                                         , destinationDirectory = siteOutput
                                         }

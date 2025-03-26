-- | 最后一步，画出整张网页。

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Aono.DrawHtml (runHTML) where

import Aono.Walker (FileInfo(..), readSortFileList)
import Aono.ArgOpt (ArgOpt (..))
import System.FilePath (takeDirectory, (</>), makeRelative, dropTrailingPathSeparator)
import Data.Time (ZonedTime(..), formatTime, defaultTimeLocale)
import Text.Hamlet (shamletFile)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze (Markup)
import qualified Data.Text.Lazy.IO as TIO (writeFile)
import Text.Lucius (luciusFile, renderCss)

import Aono.NetPath (NetPath(NetPath), joinNetPath)

type UriPath = String;

data AonoEnv = AonoEnv { aonoRootPath :: FilePath
                       , aonoFileList :: [FileInfo]
                       }

formatZonedTime :: ZonedTime -> String
formatZonedTime = formatTime defaultTimeLocale fmt
    where fmt = "%Y年%m月%d日%H时%M分%S秒"

makeEnv :: FilePath -> IO AonoEnv
makeEnv path = AonoEnv parent <$> readSortFileList (parent, NetPath [relative])
    where path' = dropTrailingPathSeparator $ dropTrailingPathSeparator path
          parent = takeDirectory path'
          relative = makeRelative parent path'

saveMarkup :: FilePath -> Markup -> IO ()
saveMarkup filepath = TIO.writeFile filepath . renderMarkup

saveCSS :: FilePath -> IO ()
saveCSS filepath = TIO.writeFile filepath $ renderCss $ $(luciusFile "./html/aono.css") undefined

-- | 整个主流程。
runHTML :: ArgOpt -> IO ()
runHTML (ArgOpt sourcePath) = do
    AonoEnv {..} <- makeEnv sourcePath
    saveCSS (aonoRootPath </> "aono.css")
    saveMarkup (aonoRootPath </> "index.html") $(shamletFile "./html/index.html")

-- | 最后一步，画出整张网页。

{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Aono.DrawHtml (runHTML) where

import RIO
import Aono.Walker (FileInfo(..), readSortFileList)
import Aono.ArgOpt (ArgOpt (..))
import RIO.FilePath (takeDirectory, hasTrailingPathSeparator, (</>), makeRelative, dropTrailingPathSeparator)
import Text.Hamlet (shamletFile)
import Text.Blaze.Renderer.Text (renderMarkup)
import Text.Blaze (Markup)
import Data.Text.Lazy.IO (writeFile)
import Text.Lucius (luciusFile, renderCss)

type UriPath = String;

data AonoEnv = AonoEnv { aonoRootPath :: FilePath
                       , aonoFileList :: [FileInfo]
                       }

makeEnv :: FilePath -> IO AonoEnv
makeEnv path = AonoEnv parent <$> readSortFileList path
    where path' = dropTrailingPathSeparator $ dropTrailingPathSeparator path
          parent = takeDirectory path'

saveMarkup :: FilePath -> Markup -> IO ()
saveMarkup filepath = writeFile filepath . renderMarkup

saveCSS :: FilePath -> IO ()
saveCSS filepath = writeFile filepath $ renderCss $ $(luciusFile "./html/aono.css") undefined

-- | 整个主流程。
runHTML :: ArgOpt -> IO ()
runHTML (ArgOpt sourcePath) = do
    AonoEnv {..} <- makeEnv sourcePath
    saveCSS (aonoRootPath </> "aono.css")
    saveMarkup (aonoRootPath </> "index.html") $(shamletFile "./html/index.html")

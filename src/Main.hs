module Main where

import Aono.ArgOpt (runArgOpt, ArgOpt (ArgOpt))
import Aono.Walker (readSortFileList)
import Control.Monad (forM_)

main :: IO ()
main = do
    ArgOpt path <- runArgOpt
    fs <- readSortFileList path
    forM_ fs print

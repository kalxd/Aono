module Main where

import Aono.ArgOpt (runArgOpt)
import Control.Monad (void)
import Aono.DrawHtml (runHTML)

main :: IO ()
main = void $ runHTML =<< runArgOpt

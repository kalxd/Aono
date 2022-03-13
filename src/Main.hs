module Main where

import Aono.ArgOpt (runArgOpt)
import Aono.DrawHtml (runHTML)

main :: IO ()
main = runHTML =<< runArgOpt

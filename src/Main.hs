module Main where

import Aono.ArgOpt (runArgOpt)

main :: IO ()
main = runArgOpt >>= print

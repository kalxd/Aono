-- | 外界参数输入部分
{-# LANGUAGE OverloadedStrings #-}
module Aono.ArgOpt ( ArgOpt (..)
                   , runArgOpt
                   ) where

import Options.Applicative (Parser, help, execParser, briefDesc)
import Options.Applicative.Builder (info, metavar, argument, str)

-- | 外界参数，包一层
newtype ArgOpt = ArgOpt String
    deriving (Show)

argParser :: Parser ArgOpt
argParser = ArgOpt <$> opt
    where opt = argument str $ metavar "Directory"

runArgOpt :: IO ArgOpt
runArgOpt = execParser p
    where p = info argParser briefDesc

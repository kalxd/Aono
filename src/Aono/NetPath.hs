{-# LANGUAGE NoImplicitPrelude #-}
-- | 网络路径
module Aono.NetPath where

import RIO
import Data.List (intercalate)

newtype NetPath = NetPath { runNetPath :: [String] }
    deriving (Show)

instance Semigroup NetPath where
  (<>) (NetPath p1) (NetPath p2) = NetPath $ p1 <> p2

instance Monoid NetPath where
  mempty = NetPath mempty

joinNetPath :: NetPath -> FilePath
joinNetPath (NetPath p) = intercalate "/" p

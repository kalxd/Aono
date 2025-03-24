-- | 网络路径
module Aono.NetPath where

import Data.List (intercalate, dropWhileEnd)
import System.FilePath (pathSeparator, joinPath, (</>), splitPath)

newtype NetPath = NetPath { runNetPath :: [String] }
    deriving (Show)

instance Semigroup NetPath where
    (<>) (NetPath p1) (NetPath p2) = NetPath $ p1 <> p2

instance Monoid NetPath where
    mempty = NetPath mempty

pathToNetPath :: FilePath -> NetPath
pathToNetPath = NetPath . map (dropWhileEnd (== pathSeparator)) . splitPath

joinNetPath :: NetPath -> FilePath
joinNetPath (NetPath p) = intercalate "/" p

pathJoinNetPath :: FilePath -> NetPath -> FilePath
pathJoinNetPath path (NetPath netpath) = path </> joinPath netpath

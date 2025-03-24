-- |工作目录漫步者
{-# LANGUAGE NoImplicitPrelude #-}
module Aono.Walker ( FileInfo(..)
                   , readSortFileList
                   ) where

import RIO
import RIO.Time (ZonedTime (zonedTimeToLocalTime), utcToZonedTime, utcToLocalZonedTime, LocalTime)
import RIO.Directory (listDirectory, getModificationTime, doesFileExist, doesDirectoryExist)
import RIO.FilePath (takeFileName, (</>))
import RIO.Text (pack)
import RIO.List (sort)
import Aono.NetPath (NetPath(..), pathJoinNetPath, pathToNetPath)

data FileInfo = FileInfo { fileTitle :: Text
                         , fileTime :: ZonedTime
                         , filePath :: NetPath
                         }
                deriving (Show)

fileInfoLocalTime :: FileInfo -> LocalTime
fileInfoLocalTime (FileInfo _ time _) = zonedTimeToLocalTime time

instance Eq FileInfo where
    (==) = on (==) fileInfoLocalTime

instance Ord FileInfo where
    compare = on compare fileInfoLocalTime

-- | 分别读取目录中的文件、目录，其它一概忽略
readDirAndFile :: FilePath -> IO ([FilePath], [FilePath])
readDirAndFile path = do
    filenames <- listDirectory path
    let fns = zip filenames $ map (path </>) filenames
    fs <- map fst <$> filterM (doesFileExist . snd) fns
    ds <- map fst <$> filterM (doesDirectoryExist .snd) fns
    pure (fs, ds)

-- | 读取文件的必要信息
readFileItem :: (FilePath, NetPath) -> IO FileInfo
readFileItem (rootpath, filepath) = do
    let fullpath = pathJoinNetPath rootpath filepath
    time <- utcToLocalZonedTime =<< getModificationTime fullpath
    let filename = takeFileName fullpath
    pure $ FileInfo (pack filename) time filepath

-- | 遍历整个目录
workDirDeeply :: (FilePath, NetPath) -> IO [FileInfo]
workDirDeeply (rootPath, relative) = do
    (fs, ds) <- readDirAndFile $ pathJoinNetPath rootPath relative
    fs' <- mapM (readFileItem . (,) rootPath .  (relative <>) . pathToNetPath) fs
    fss' <- concat <$> mapM (\d -> workDirDeeply (rootPath, relative <> NetPath [d])) ds
    pure $ fs' ++ fss'

-- | 从目录中读出排过序的文件。
readSortFileList :: (FilePath, NetPath) -> IO [FileInfo]
readSortFileList x = reverse . sort <$> workDirDeeply x

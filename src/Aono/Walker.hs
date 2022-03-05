-- |工作目录漫步者
{-# LANGUAGE NoImplicitPrelude #-}
module Aono.Walker ( FileInfo(..)
                   , readSortFileList
                   ) where

import RIO
import RIO.Time (UTCTime)
import RIO.Directory (listDirectory, getModificationTime, doesFileExist, doesDirectoryExist)
import RIO.FilePath (takeFileName, (</>))
import RIO.Text (pack)
import RIO.List (sort)

type LastModfiedTime = UTCTime

data FileInfo = FileInfo { fileTitle :: Text
                         , fileTime :: UTCTime
                         , filePath :: FilePath
                         }
                deriving (Show)

newtype FileItem = FileItem FileInfo

instance Eq FileItem where
    (FileItem f1) ==  (FileItem f2) = fileTime f1 == fileTime f2

instance Ord FileItem where
    compare (FileItem f1) (FileItem f2) = compare (fileTime f1) (fileTime f2)

-- | 分别读取目录中的文件、目录，其它一概忽略
readDirAndFile :: FilePath -> IO ([FilePath], [FilePath])
readDirAndFile path = do
    xs <- listDirectory path
    fs <- filterM doesFileExist $ map (path </>) xs
    ds <- filterM doesDirectoryExist $ map (path </>) xs
    pure (fs, ds)

-- | 读取文件的必要信息
readFileItem :: FilePath -> IO FileItem
readFileItem filepath = do
    time <- getModificationTime filepath
    let filename = takeFileName filepath
    pure $ FileItem $ FileInfo (pack filename) time filepath

-- | 遍历整个目录
workDirDeeply :: FilePath -> IO [FileItem]
workDirDeeply dirpath = do
    (fs, ds) <- readDirAndFile dirpath
    fs' <- mapM readFileItem fs
    fss' <- concat <$> mapM workDirDeeply ds
    pure $ fs' ++ fss'

-- | 从目录中读出排过序的文件。
readSortFileList :: FilePath -> IO [FileInfo]
readSortFileList path = do
    fs <- reverse . sort <$> workDirDeeply path
    let unwrap (FileItem info) = info
    pure $ map unwrap fs

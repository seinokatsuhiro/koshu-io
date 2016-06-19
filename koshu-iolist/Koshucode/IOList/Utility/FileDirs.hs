{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Utility.FileDirs
 ( -- * Data
   RevDirs, FileDirs (..),
   fileDirs, fileDirsPath,
   filePush, filePushTo,
   fromFileName, fromFileNameRevDirs,
   changeExtension,
 ) where

import qualified System.FilePath.Posix                 as Posix
import qualified Koshucode.IOList.Utility.FilePath     as K

-- | Reversed names of directories
type RevDirs = [FilePath]

data FileDirs = FileDirs
    { fileName     :: FilePath    -- ^ Filename
    , fileRevDirs  :: RevDirs     -- ^ Reversed names of directories
    } deriving (Show, Eq, Ord)

fileDirs :: FileDirs -> [FilePath]
fileDirs FileDirs {..} = K.dropDot $ reverse $ fileName : fileRevDirs

fileDirsPath :: FileDirs -> FilePath
fileDirsPath f = K.slash $ fileDirs f

filePush :: FilePath -> FileDirs -> FileDirs
filePush path fd = FileDirs path (fileName fd : fileRevDirs fd)

filePushTo :: FileDirs -> FilePath -> FileDirs
filePushTo = flip filePush

fromFileName :: FilePath -> FileDirs
fromFileName name = FileDirs name []

fromFileNameRevDirs :: FilePath -> RevDirs -> FileDirs
fromFileNameRevDirs = FileDirs

-- | Change filename extension
changeExtension :: String -> FileDirs -> FileDirs
changeExtension ext file@FileDirs {..} =
    file { fileName = Posix.dropExtension fileName ++ "." ++ ext }


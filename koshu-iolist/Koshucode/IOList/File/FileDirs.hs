{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | File and directory names.

module Koshucode.IOList.File.FileDirs
 ( -- * Data
   FileDirs (..), RevDirs,

   -- * Functions
   fileDirs,
   fileDirsPathList, fileDirsPath,
   filePush, filePushTo,
   changeExtension,
 ) where

import qualified System.FilePath.Posix                 as Posix
import qualified Koshucode.IOList.File.FilePath        as K

-- | Reversed names of directories
type RevDirs = [FilePath]

-- | Filename and directory names.
data FileDirs = FileDirs
    { fileName     :: FilePath    -- ^ Filename
    , fileRevDirs  :: RevDirs     -- ^ Reversed names of directories
    } deriving (Show, Eq, Ord)

-- | Construct 'FileDirs' from file name (not include directories).
fileDirs :: FilePath -> FileDirs
fileDirs name = FileDirs name []

-- | Components of path of file.
fileDirsPathList :: FileDirs -> [FilePath]
fileDirsPathList FileDirs {..} = K.dropDot $ reverse $ fileName : fileRevDirs

-- | Path of file.
fileDirsPath :: FileDirs -> FilePath
fileDirsPath f = K.slash $ fileDirsPathList f

-- | Append filename.
filePush :: FilePath -> FileDirs -> FileDirs
filePush path fd = FileDirs path (fileName fd : fileRevDirs fd)

-- | Append filename.
filePushTo :: FileDirs -> FilePath -> FileDirs
filePushTo = flip filePush

-- | Change filename extension
changeExtension :: String -> FileDirs -> FileDirs
changeExtension ext file@FileDirs {..} =
    file { fileName = Posix.dropExtension fileName ++ "." ++ ext }


{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

-- | File and directory names.

module Koshucode.IOList.File.FileDirs
 ( -- * Data
   FileDirs (..), RevDirs,

   -- * Functions
   fromFileName, fromFileNameRevDirs,
   fileDirs, fileDirsPath,
   filePush, filePushTo,
   changeExtension,
 ) where

import qualified System.FilePath.Posix                 as Posix
import qualified Koshucode.IOList.File.FilePath        as K

-- | Reversed names of directories
type RevDirs = [FilePath]

data FileDirs = FileDirs
    { fileName     :: FilePath    -- ^ Filename
    , fileRevDirs  :: RevDirs     -- ^ Reversed names of directories
    } deriving (Show, Eq, Ord)

-- | Construct 'FileDirs' from file name (not include directories).
fromFileName :: FilePath -> FileDirs
fromFileName name = FileDirs name []

-- | Construct 'FileDirs' from file name and reversed directories.
fromFileNameRevDirs :: FilePath -> RevDirs -> FileDirs
fromFileNameRevDirs = FileDirs

fileDirs :: FileDirs -> [FilePath]
fileDirs FileDirs {..} = K.dropDot $ reverse $ fileName : fileRevDirs

fileDirsPath :: FileDirs -> FilePath
fileDirsPath f = K.slash $ fileDirs f

filePush :: FilePath -> FileDirs -> FileDirs
filePush path fd = FileDirs path (fileName fd : fileRevDirs fd)

filePushTo :: FileDirs -> FilePath -> FileDirs
filePushTo = flip filePush

-- | Change filename extension
changeExtension :: String -> FileDirs -> FileDirs
changeExtension ext file@FileDirs {..} =
    file { fileName = Posix.dropExtension fileName ++ "." ++ ext }


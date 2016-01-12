{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.FileDirs
 ( -- * For files
   RevDirs, FileDirs (..),
   fileDirs, fileDirsPath,
   filePush,
   fromFileName, fromFileNameRevDirs,
   slash, slashSpace,
   cutDot,
   changeExtension,
 ) where

import qualified Data.List                 as List
import qualified System.FilePath.Posix     as Posix

-- | Reversed names of directories
type RevDirs = [FilePath]

data FileDirs = FileDirs
    { fileName     :: FilePath    -- ^ Filename
    , fileRevDirs  :: RevDirs     -- ^ Reversed names of directories
    } deriving (Show, Eq, Ord)

fileDirs :: FileDirs -> [FilePath]
fileDirs FileDirs {..} = cutDot $ reverse $ fileName : fileRevDirs

fileDirsPath :: FileDirs -> FilePath
fileDirsPath f = slash $ fileDirs f

filePush :: FilePath -> FileDirs -> FileDirs
filePush path fd = FileDirs path (fileName fd : fileRevDirs fd)

fromFileName :: FilePath -> FileDirs
fromFileName name = FileDirs name []

fromFileNameRevDirs :: FilePath -> RevDirs -> FileDirs
fromFileNameRevDirs = FileDirs

slash :: [FilePath] -> String
slash = List.intercalate "/"

slashSpace :: [FilePath] -> String
slashSpace = List.intercalate "/ "

cutDot :: [FilePath] -> [FilePath]
cutDot ("." : ps) = ps
cutDot ps         = ps

changeExtension :: String -> FileDirs -> FileDirs
changeExtension ext file@FileDirs {..} =
    file { fileName = Posix.dropExtension fileName ++ "." ++ ext }


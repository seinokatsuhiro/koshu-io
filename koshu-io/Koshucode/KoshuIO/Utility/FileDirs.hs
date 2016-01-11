{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.FileDirs
 ( -- * For files
   RevDirs, FileDirs (..),
   fileDirs, fileDirsPath,
   fromFileName, fromFileNameRevDirs,
   slash, slashSpace,
   cutDot,
 ) where

import qualified Data.List   as List

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


{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.FilePath
 ( -- * Pure functions
   slash, slashSpace,
   dropDot, omitHidden, isHiddenPath,

   -- * Functions with I/O
   directoryOrNot,
   listDirectory,
 ) where

import qualified Data.List                 as List
import qualified System.Directory          as Dir


----------------------  Pure functions

-- | Join filenames with slash.
slash :: [FilePath] -> String
slash = List.intercalate "/"

-- | Join filenames with slash plus space.
slashSpace :: [FilePath] -> String
slashSpace = List.intercalate "/ "

-- | Drop leading dot.
dropDot :: [FilePath] -> [FilePath]
dropDot ("." : ps) = dropDot ps
dropDot ps         = ps

omitHidden :: [FilePath] -> [FilePath]
omitHidden = filter (not . isHiddenPath)

isHiddenPath :: String -> Bool
isHiddenPath ('.' : _ : _)  = True
isHiddenPath _              = False


----------------------  Functions with I/O

-- | Split paths into directories and non-directories.
directoryOrNot :: [FilePath] -> IO ([FilePath], [FilePath])
directoryOrNot = loop where
    loop [] = return ([], [])
    loop (p:ps) = do (ds, fs) <- loop ps
                     exist <- Dir.doesDirectoryExist p
                     case exist of
                       True  -> return (p:ds, fs)
                       False -> return (ds, p:fs)

-- | Sorted paths of directory contents.
listDirectory :: FilePath -> IO [FilePath]
listDirectory path =
    do fs <- Dir.listDirectory path
       return $ List.sort fs



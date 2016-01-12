{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.ForFiles
 ( -- * For files
   forFiles, forFilesRec, forFilesRec_, forFilesPrint,
   forFilesAction,
   -- * Directory action
   DirAction, DirFilter, 
   dirActionAll, dirActionVisible, dirActionPrint,
   -- * File action
   FileAction,
   fileActionNull, fileActionPrint,
   -- * Utility
   directoryOrNot,
   omitHidden,
 ) where

import qualified Data.List                            as List
import qualified System.Directory                     as Dir
import qualified Koshucode.KoshuIO.Utility.FileDirs   as K


-- ----------------------  driver

type DirFilter = DirAction [FilePath]

type DirAction a = K.FileDirs -> [FilePath] -> IO a

type FileAction a = K.FileDirs -> IO a

forFilesRec_ :: DirFilter -> FileAction a -> FilePath -> IO ()
forFilesRec_ dir file path = forFilesRec dir file path >> return ()

forFilesRec :: DirFilter -> FileAction a -> FilePath -> IO [a]
forFilesRec dir file = loop [] where
    loop up path = do
      let f = K.FileDirs path up
      exist <- Dir.doesDirectoryExist path
      case exist of
        False -> do r <- file f
                    return [r]
        True  -> forFiles path $ \files -> do
                    let up' = path : up
                    files' <- dir f files
                    rs     <- loop up' `mapM` files'
                    return $ concat rs

forFilesAction :: DirAction [a] -> K.FileDirs -> IO [a]
forFilesAction dir = loop where
    loop fd = do
      let path = K.fileName fd
      exist <- Dir.doesDirectoryExist path
      case exist of
        False -> return []
        True  -> forFiles path $ dir fd

forFiles :: FilePath -> ([FilePath] -> IO a) -> IO a
forFiles path f = do
  files <- sortedFiles path
  Dir.withCurrentDirectory path $ f files

sortedFiles :: FilePath -> IO [FilePath]
sortedFiles path =
    do fs <- Dir.listDirectory path
       return $ List.sort fs

forFilesPrint :: FilePath -> IO ()
forFilesPrint path = forFilesRec_ dirActionPrint fileActionPrint path

directoryOrNot :: [FilePath] -> IO ([FilePath], [FilePath])
directoryOrNot = loop where
    loop [] = return ([], [])
    loop (p:ps) = do (ds, fs) <- loop ps
                     exist <- Dir.doesFileExist p
                     case exist of
                       False -> return (ds, p:fs)
                       True  -> return (p:ds, fs)


-- ----------------------  actions

dirActionAll :: DirFilter
dirActionAll _ = return

dirActionVisible :: DirFilter
dirActionVisible = dirActionBy omitHidden

dirActionBy :: ([FilePath] -> [FilePath]) -> DirFilter
dirActionBy keep _ files = return $ keep files

dirActionPrint :: DirFilter
dirActionPrint dir files = do
  putStrLn $ unwords (reverse $ show (length files) : "/" : K.fileRevDirs dir)
  return files

fileActionNull :: FileAction ()
fileActionNull _ = return ()

fileActionPrint :: FileAction ()
fileActionPrint file = do
  putStrLn $ unwords (reverse $ K.fileName file : K.fileRevDirs file)


-- ----------------------  utility

omitHidden :: [FilePath] -> [FilePath]
omitHidden = filter (not . isHidden)

isHidden :: String -> Bool
isHidden ('.' : _ : _)  = True
isHidden _              = False

-- omitDots :: [FilePath] -> [FilePath]
-- omitDots ps = filter notDots ps where
--     notDots s = s /= "." && s /= ".."


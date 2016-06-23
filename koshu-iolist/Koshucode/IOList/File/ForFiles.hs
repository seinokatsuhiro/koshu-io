{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.File.ForFiles
 ( -- * For files
   forFiles, forFilesRec, forFilesRec_, forFilesPrint,
   forFilesUp,

   -- * Directory action
   DirAction, DirFilter, 
   dirActionAll, dirActionVisible, dirActionPrint,

   -- * File action
   FileAction,
   fileActionNull, fileActionPrint,
 ) where

import qualified System.Directory                    as Dir
import qualified Koshucode.IOList.File.FileDirs      as K
import qualified Koshucode.IOList.File.FilePath      as K


-- ----------------------  For files

-- | @forFiles@ /dir/ /action/ applies action to filenames in given directory.
forFiles :: FilePath -> ([FilePath] -> IO a) -> IO a
forFiles path act = do
  files <- K.listDirectory path
  Dir.withCurrentDirectory path $ act files

forFilesUp :: forall a. FilePath -> DirAction ([a], [FilePath]) -> IO [a]
forFilesUp p act = loop $ K.fileDirs p where
    loop :: K.FileDirs -> IO [a]
    loop up = forFiles (K.fileName up) $ \files -> do
         (rs, fs) <- act up files
         (ds, _)  <- K.directoryOrNot fs
         rs'      <- loop `mapM` map (K.filePushTo up) ds
         return $ rs ++ concat rs'

forFilesRec :: forall a. FilePath -> DirFilter -> FileAction a -> IO [a]
forFilesRec p dir file = forFilesUp p act where
    act :: DirAction ([a], [FilePath])
    act up files = do
      files' <- dir up files
      (ds, fs) <- K.directoryOrNot files'
      rs <- file `mapM` map (K.filePushTo up) fs
      return (rs, ds)

forFilesRec_ :: FilePath -> DirFilter -> FileAction a -> IO ()
forFilesRec_ path dir file = forFilesRec path dir file >> return ()

forFilesPrint :: FilePath -> IO ()
forFilesPrint path = forFilesRec_ path dirActionPrint fileActionPrint


-- --------------------------------------------  Directory action

-- | Action for directory and content files.
type DirAction a = K.FileDirs -> [FilePath] -> IO a

type DirFilter = DirAction [FilePath]

dirActionAll :: DirFilter
dirActionAll _ = return

dirActionVisible :: DirFilter
dirActionVisible = dirActionBy K.omitHidden

dirActionBy :: ([FilePath] -> [FilePath]) -> DirFilter
dirActionBy keep _ files = return $ keep files

-- | Print number of files and directory path.
dirActionPrint :: DirFilter
dirActionPrint dir files = do
  putStrLn $ unwords (reverse $ show (length files) : "/" : K.fileRevDirs dir)
  return files


-- --------------------------------------------  File action

-- | Action for file.
type FileAction a = K.FileDirs -> IO a

-- | Do nothing.
fileActionNull :: FileAction ()
fileActionNull _ = return ()

-- | Print file path.
fileActionPrint :: FileAction ()
fileActionPrint file = do
  putStrLn $ unwords (reverse $ K.fileName file : K.fileRevDirs file)


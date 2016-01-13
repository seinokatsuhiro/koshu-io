{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.ForFiles
 ( -- * For files
   forFiles, forFilesRec, forFilesRec_, forFilesPrint,
   forFilesUp,
   -- * Directory action
   DirAction, DirFilter, 
   dirActionAll, dirActionVisible, dirActionPrint,
   -- * File action
   FileAction,
   fileActionNull, fileActionPrint,
   -- * Utility
   directoryOrNot,
   listDir,
   omitHidden,
 ) where

import qualified Data.List                            as List
import qualified System.Directory                     as Dir
import qualified Koshucode.KoshuIO.Utility.FileDirs   as K


-- ----------------------  driver

type DirFilter = DirAction [FilePath]

type DirAction a = K.FileDirs -> [FilePath] -> IO a

type FileAction a = K.FileDirs -> IO a

forFiles :: FilePath -> ([FilePath] -> IO a) -> IO a
forFiles path act = do
  files <- listDir path
  Dir.withCurrentDirectory path $ act files

forFilesUp :: forall a. FilePath -> DirAction ([a], [FilePath]) -> IO [a]
forFilesUp p act = loop $ K.fromFileName p where
    loop :: K.FileDirs -> IO [a]
    loop up = forFiles (K.fileName up) $ \files -> do
         (rs, fs) <- act up files
         (ds, _)  <- directoryOrNot fs
         rs'      <- loop `mapM` map (`K.filePush` up) ds
         return $ rs ++ concat rs'

forFilesRec :: FilePath -> DirFilter -> FileAction a -> IO [a]
forFilesRec p dir file = loop [] p where
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

forFilesRec_ :: FilePath -> DirFilter -> FileAction a -> IO ()
forFilesRec_ path dir file = forFilesRec path dir file >> return ()

forFilesPrint :: FilePath -> IO ()
forFilesPrint path = forFilesRec_ path dirActionPrint fileActionPrint


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

directoryOrNot :: [FilePath] -> IO ([FilePath], [FilePath])
directoryOrNot = loop where
    loop [] = return ([], [])
    loop (p:ps) = do (ds, fs) <- loop ps
                     exist <- Dir.doesDirectoryExist p
                     case exist of
                       True  -> return (p:ds, fs)
                       False -> return (ds, p:fs)

listDir :: FilePath -> IO [FilePath]
listDir path =
    do fs <- Dir.listDirectory path
       return $ List.sort fs

omitHidden :: [FilePath] -> [FilePath]
omitHidden = filter (not . isHidden)

isHidden :: String -> Bool
isHidden ('.' : _ : _)  = True
isHidden _              = False

-- omitDots :: [FilePath] -> [FilePath]
-- omitDots ps = filter notDots ps where
--     notDots s = s /= "." && s /= ".."


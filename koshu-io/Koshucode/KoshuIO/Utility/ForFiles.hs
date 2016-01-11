{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Utility.ForFiles
 ( -- * For files
   forFiles, forFiles_, forFilesPrint,
   -- * Directory action
   DirAction, 
   dirActionAll, dirActionVisible, dirActionPrint,
   -- * File action
   FileAction,
   fileActionNull, fileActionPrint,
   -- * Utility
   omitHidden,
 ) where

import qualified System.Directory                     as Dir
import qualified Koshucode.KoshuIO.Utility.FileDirs   as K


-- ----------------------  driver

type DirAction = K.RevDirs -> [FilePath] -> IO [FilePath]

type FileAction a = K.FileDirs -> IO a

forFiles_ :: DirAction -> FileAction a -> FilePath -> IO ()
forFiles_ dir file path = forFiles dir file path >> return ()

forFiles :: DirAction -> FileAction a -> FilePath -> IO [a]
forFiles dir file = loop [] where
    loop up path = do
      exist <- Dir.doesDirectoryExist path
      case exist of
        False -> do r <- file $ K.FileDirs path up
                    return [r]
        True  -> do files <- Dir.getDirectoryContents path
                    Dir.withCurrentDirectory path $ do
                      let up' = path : up
                      files' <- dir up' $ omitDots files
                      rs     <- loop up' `mapM` files'
                      return $ concat rs
  
forFilesPrint :: FilePath -> IO ()
forFilesPrint path = forFiles_ dirActionPrint fileActionPrint path


-- ----------------------  actions

dirActionAll :: DirAction
dirActionAll _ = return

dirActionVisible :: DirAction
dirActionVisible = dirActionBy omitHidden

dirActionBy :: ([FilePath] -> [FilePath]) -> DirAction
dirActionBy keep _ files = return $ keep files

dirActionPrint :: DirAction
dirActionPrint dirs files = do
  putStrLn $ unwords (reverse $ show (length files) : "/" : dirs)
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

omitDots :: [FilePath] -> [FilePath]
omitDots ps = filter notDots ps where
    notDots s = s /= "." && s /= ".."


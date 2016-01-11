{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.IOList.File
 ( File (..), maybeFile,
   fileSection, fileSections,
 ) where

import qualified Data.ByteString                    as Bs
import qualified Data.Maybe                         as Maybe
import qualified System.Directory                   as Dir

import qualified Koshucode.KoshuIO.IOList.Markdown  as K
import qualified Koshucode.KoshuIO.IOList.Section   as K


data File = File 
    { filePath     :: FilePath         -- ^ Path of file
    , fileContent  :: Bs.ByteString    -- ^ Content of file
    } deriving (Show, Eq, Ord)

instance K.Numbering File where
    numbering a = a

instance K.ToMarkdown File where
    toMarkdown (File _ bs) = do
        K.mdBlock bs

-- | Read and craete 'File' if it exists.
maybeFile :: FilePath -> IO (Maybe File)
maybeFile path = do
  exist <- Dir.doesFileExist path
  case exist of
    False  -> return Nothing
    True   -> do content <- Bs.readFile path
                 return $ Just $ File path content

-- | Create section for input file.
fileSection :: FilePath -> IO (Maybe (K.Section File))
fileSection path = do
  file <- maybeFile path
  return $ K.subsection path `fmap` file

fileSections :: [FilePath] -> IO [K.Section File]
fileSections ps =
    do fs <- fileSection `mapM` ps
       return $ Maybe.catMaybes fs


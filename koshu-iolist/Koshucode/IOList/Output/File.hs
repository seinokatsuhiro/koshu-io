{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Output.File
 ( File (..), maybeFile,
   fileSection, fileSections,
 ) where

import qualified Data.ByteString                   as Bs
import qualified Data.Maybe                        as Maybe
import qualified System.Directory                  as Dir
import qualified Koshucode.Baala.Base              as K

import qualified Koshucode.IOList.Output.Markdown  as K
import qualified Koshucode.IOList.Output.Section   as K


data File = File 
    { filePath     :: FilePath     -- ^ Path of file
    , fileContent  :: K.Bs         -- ^ Content of file
    } deriving (Show, Eq, Ord)

instance K.Numbering File where
    numbering a = a

instance K.ToMarkdown File where
    toMarkdown (File _ bs) = K.mdBlock bs

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


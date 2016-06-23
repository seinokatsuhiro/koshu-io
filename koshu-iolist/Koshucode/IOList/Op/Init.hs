{-# OPTIONS_GHC -Wall #-}

-- | Create I/O script.

module Koshucode.IOList.Op.Init
 ( opSummaryInit,
   opGrandInit,
 ) where

import qualified System.Directory             as Dir
import qualified Koshucode.IOList.Parts       as K

-- | @summary-init@ -- Create summary script.
opSummaryInit :: K.Op
opSummaryInit = initOf "summary"

-- | @grand-init@ -- Create grand summary script.
opGrandInit :: K.Op
opGrandInit = initOf "grand"

initOf :: String -> K.Op
initOf name _ []         = initHelp name
initOf name _ (file : _) = do
  exist <- Dir.doesFileExist file
  case exist of
    False -> do writeFile file $ unlines ["iolist " ++ name ++ " " ++ file]
                return K.StatusMessage
    True  -> do putStrLn $ "ABORT -- File already exists: " ++ file
                return K.StatusMessage

initHelp :: String -> IO K.Status
initHelp name = do
  putStrLn $ "USAGE"
  putStrLn $ "  iolist " ++ name ++ "-init IOLIST"
  putStrLn $ ""
  return K.StatusMessage


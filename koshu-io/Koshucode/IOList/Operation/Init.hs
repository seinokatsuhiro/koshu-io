{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Operation.Init
 ( opSummaryInit,
   opGrandInit,
 ) where

import qualified System.Directory             as Dir
import qualified Koshucode.IOList.Param       as K
import qualified Koshucode.IOList.Status      as K

-- | Operation for @summary-init@
opSummaryInit :: K.Operation
opSummaryInit = initOf "summary"

-- | Operation for @grand-init@
opGrandInit :: K.Operation
opGrandInit = initOf "grand"

initOf :: String -> K.Operation
initOf name _ []         = initHelp name
initOf name _ (file : _) = do
  exist <- Dir.doesFileExist file
  case exist of
    False -> do writeFile file $ unlines ["koshu-io " ++ name ++ " " ++ file]
                return K.StatusMessage
    True  -> do putStrLn $ "ABORT -- File already exists: " ++ file
                return K.StatusMessage

initHelp :: String -> IO K.Status
initHelp name = do
  putStrLn $ "USAGE"
  putStrLn $ "  koshu-io " ++ name ++ "-init IOLIST"
  putStrLn $ ""
  return K.StatusMessage


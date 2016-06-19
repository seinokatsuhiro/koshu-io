{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Operate
 ( Operation',
   GetOperations (..),
   operate,
 ) where

import qualified Koshucode.IOList.Utility     as K
import qualified Koshucode.IOList.Status      as K

-- | Common type for command-line operation.
type Operation' p = p                  -- ^ Parameter
                 -> [K.CmdArg]         -- ^ Command-line arguments
                 -> IO K.Status        -- ^ Result

-- | Function for getting list of operations.
class GetOperations p where
    getOperations :: p -> [K.Assoc (Operation' p)]

-- | Execute command-line operation.
operate :: (GetOperations p) => Operation' p
operate _ [] = help
operate p (name : args) =
    case K.assocPrefix name $ getOperations p of
      [K.Assoc _ op] -> op p args
      []             -> help
      ops            -> ambiguous $ map K.assocKey ops

ambiguous :: [String] -> IO K.Status
ambiguous ns = do
  putStrLn "MESSAGE"
  putStrLn "  Ambiguous operation"
  putStrLn ""
  putStrLn "DETAIL"
  mapM_ putName ns
  putStrLn ""
  return K.StatusMessage

putName :: String -> IO ()
putName n = do
  putStr "  "
  putStrLn n

help :: IO K.Status
help = do
  putStrLn "DESCRIPTION"
  putStrLn "  Generate and compare I/O list"
  putStrLn ""
  putStrLn "USAGE"
  putStrLn "  (Typical) koshu-io run IOLIST"
  putStrLn "  (General) koshu-io OPERATION"
  putStrLn ""
  putStrLn "OPERATION"
  putStrLn "  command       CMD ARG ..."
  putStrLn "  command       CMD ARG ... // FILE ..."
  putStrLn "  find          IOLIST ..."
  putStrLn "  grand         IOLIST ..."
  putStrLn "  grand-init    IOLIST"
  putStrLn "  run           IOLIST"
  putStrLn "  summary       IOLIST ..."
  putStrLn "  summary-init  IOLIST"
  putStrLn ""
  return K.StatusMessage

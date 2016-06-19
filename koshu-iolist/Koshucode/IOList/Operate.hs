{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Operate
 ( Operation',
   GetOperations (..),
   operate,
 ) where

import Data.Monoid ((<>))
import qualified Koshucode.Baala.Base         as K
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

-- | Print ambiguous message.
ambiguous :: [String] -> IO K.Status
ambiguous ns =
    do K.putMixLines K.lfBreak msg
       return K.StatusMessage
    where
      msg = [ K.mixBs "MESSAGE"
            , K.mixBs "  Ambiguous operation"
            , K.mixBs ""
            , K.mixBs "DETAIL"
            , mconcat $ name <$> ns
            ]

      name n = (K.mix2 <> K.mix n <> K.mixHard)

-- | Print help message.
help :: IO K.Status
help =
    do K.putMixLines K.lfBreak msg
       return K.StatusMessage
    where
      msg = [ K.mixBs "DESCRIPTION"
            , K.mixBs "  Generate and compare I/O list"
            , K.mixBs ""
            , K.mixBs "USAGE"
            , K.mixBs "  (Typical) iolist run IOLIST"
            , K.mixBs "  (General) iolist OPERATION"
            , K.mixBs ""
            , K.mixBs "OPERATION"
            , K.mixBs "  command       CMD ARG ..."
            , K.mixBs "  command       CMD ARG ... // FILE ..."
            , K.mixBs "  find          IOLIST ..."
            , K.mixBs "  grand         IOLIST ..."
            , K.mixBs "  grand-init    IOLIST"
            , K.mixBs "  run           IOLIST"
            , K.mixBs "  summary       IOLIST ..."
            , K.mixBs "  summary-init  IOLIST"
            , K.mixBs ""
            ]

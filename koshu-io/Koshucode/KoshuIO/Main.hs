{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Main (koshuIO) where

import qualified System.Environment              as Env

import qualified Koshucode.KoshuIO.Operate       as K
import qualified Koshucode.KoshuIO.Operation     as K
import qualified Koshucode.KoshuIO.Param         as K
import qualified Koshucode.KoshuIO.Status        as K
import qualified Koshucode.KoshuIO.Utility       as K

-- | Main function for @koshu-io@ command.
koshuIO :: IO ()
koshuIO = do
  args  <- Env.getArgs
  p     <- K.param
  K.command $ K.operate (setupParam p) args

setupParam :: K.Param -> K.Param
setupParam p =
    p { K.paramAuthor      = Nothing
      , K.paramDateTime    = Nothing
      , K.paramOperations  = operations
      }

operations :: [K.Assoc K.Operation]
operations = 
    [ K.assoc "command"       K.opCommand
    , K.assoc "find"          K.opFind
    , K.assoc "grand"         K.opGrand
    , K.assoc "grand-init"    K.opGrandInit
    , K.assoc "run"           K.opRun
    , K.assoc "summary"       K.opSummary
    , K.assoc "summary-init"  K.opSummaryInit
    ]


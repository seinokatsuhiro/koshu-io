{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Main (iolistMain) where

import qualified System.Environment              as Env

import qualified Koshucode.IOList.Operate       as K
import qualified Koshucode.IOList.Operation     as K
import qualified Koshucode.IOList.Param         as K
import qualified Koshucode.IOList.Status        as K
import qualified Koshucode.IOList.Utility       as K

-- | Main function for @iolist@ command.
iolistMain :: IO ()
iolistMain = do
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
    [ K.assoc "cmd"           K.opCmd
    , K.assoc "find"          K.opFind
    , K.assoc "grand"         K.opGrand
    , K.assoc "grand-init"    K.opGrandInit
    , K.assoc "run"           K.opRun
    , K.assoc "summary"       K.opSummary
    , K.assoc "summary-init"  K.opSummaryInit
    ]


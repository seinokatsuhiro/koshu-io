{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Main
  ( iolistMain ) where

import qualified Koshucode.IOList.Assoc         as K
import qualified Koshucode.IOList.Operation     as K
import qualified Koshucode.IOList.Op            as K
import qualified Koshucode.IOList.Param         as K
import qualified Koshucode.IOList.Status        as K

-- | Main function for @iolist@ command.
iolistMain :: IO ()
iolistMain =
  do p <- K.param
     K.command $ K.operate (setupParam p) (K.paramArgs p)

setupParam :: K.Param -> K.Param
setupParam p =
    p { K.paramAuthor      = Nothing
      , K.paramDateTime    = Nothing
      , K.paramOps         = ops
      }

ops :: [K.Assoc K.Op]
ops = 
    [ K.assoc "cmd"           K.opCmd
    , K.assoc "find"          K.opFind
    , K.assoc "grand"         K.opGrand
    , K.assoc "grand-init"    K.opGrandInit
    , K.assoc "run"           K.opRun
    , K.assoc "summary"       K.opSummary
    , K.assoc "summary-init"  K.opSummaryInit
    ]


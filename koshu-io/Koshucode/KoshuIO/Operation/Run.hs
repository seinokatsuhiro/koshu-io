{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.KoshuIO.Operation.Run
 ( opCommand, opRun,
   runScript,
 ) where

import qualified Data.ByteString.Lazy                 as Bz
import qualified System.IO                            as IO

import qualified Koshucode.KoshuIO.IOList             as K
import qualified Koshucode.KoshuIO.Operate            as K
import qualified Koshucode.KoshuIO.Param              as K
import qualified Koshucode.KoshuIO.Status             as K
import qualified Koshucode.KoshuIO.Utility            as K
import qualified Koshucode.KoshuIO.Operation.Regress  as K

-- | Operation for @command@
opCommand :: K.Operation
opCommand p args = do
  bz <- K.ioList p [unwords args]
  Bz.hPut IO.stdout bz
  return K.StatusMessage

-- | Operation for @run@
opRun :: K.Operation
opRun p [file] = runScript p $ K.fromFileName file
opRun _ _      = helpRun

helpRun :: IO K.Status
helpRun = do
  putStrLn "MESSAGE"
  putStrLn "  Require one filename"
  putStrLn ""
  putStrLn "USAGE"
  putStrLn "  koshu-io run IOFILE"
  putStrLn ""
  return K.StatusMessage

runScript :: K.Param -> K.FileDirs -> IO K.Status
runScript p f = do
  let file  = K.fileName f
      p'    = K.paramSetScript p f
  cmds <- K.readCommand file
  case cmds of
    [cmd] | begin ["grand"]    -> K.operate (K.paramSetGrand p f) cmd'
          | begin ["summary"]  -> K.operate (K.paramSetSummary p f) cmd'
          | begin []           -> K.operate p' cmd'
        where begin ws = K.beginWith ("koshu-io" : ws) cmd
              cmd'     = tail $ words cmd
    _ -> runIOList p' cmds

runIOList :: K.Param -> [K.CmdLine] -> IO K.Status
runIOList p cmds = retry where
    retry = do
      io <- K.ioList p cmds
      K.saveOrRegress p io retry


{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.Operation.Run
 ( opCmd, opRun,
   runScriptFile,
   runScriptContent,
 ) where

import qualified System.IO                           as IO
import qualified Koshucode.Baala.Base                as K

import qualified Koshucode.IOList.Output             as K
import qualified Koshucode.IOList.Operate            as K
import qualified Koshucode.IOList.Param              as K
import qualified Koshucode.IOList.Status             as K
import qualified Koshucode.IOList.Utility            as K
import qualified Koshucode.IOList.Operation.Regress  as K

-- | Operation for @command@
opCmd :: K.Operation
opCmd p args = do
  mx <- K.ioList p [unwords args]
  K.hPutMix K.crlfBreak IO.stdout mx
  return K.StatusMessage

-- | Operation for @run@
opRun :: K.Operation
opRun p args =
    case args of
      [file] -> run file
      []     -> run $ K.paramDefault p
      _      -> helpRun
    where
      run file = runScriptFile p $ K.fromFileName file

helpRun :: IO K.Status
helpRun = do
  putStrLn "MESSAGE"
  putStrLn "  Require one filename"
  putStrLn ""
  putStrLn "USAGE"
  putStrLn "  iolist run [IOFILE]"
  putStrLn ""
  return K.StatusMessage

-- | Run I/O script.
runScriptFile :: K.Param -> K.FileDirs -> IO K.Status
runScriptFile p f = do
  script <- K.readScript f $ K.fileName f
  runScriptContent p script

-- | Run I/O script.
runScriptContent :: K.Param -> K.Script -> IO K.Status
runScriptContent p script =
  case script of
    K.CommandScript fd cmds -> runIOList (K.paramSetScript  p fd)  cmds
    K.SummaryScript fd cmd  -> K.operate (K.paramSetSummary p fd) $ tail $ words cmd
    K.GrandScript   fd cmd  -> K.operate (K.paramSetGrand   p fd) $ tail $ words cmd

runIOList :: K.Param -> [K.CmdLine] -> IO K.Status
runIOList p cmds = retry where
    retry = do
      io <- K.ioList p cmds
      K.saveOrRegress p (K.mixToBz K.crlfBreak io) retry


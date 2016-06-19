{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}

module Koshucode.IOList.IOList.IOList
 ( ioList,
 ) where

import           Data.List                         ((\\))
import qualified System.FilePath.Glob              as Glob
import qualified System.Process.ByteString         as Proc

import qualified Koshucode.Baala.Base              as K

import qualified Koshucode.IOList.Param            as K
import qualified Koshucode.IOList.Utility          as K
import qualified Koshucode.IOList.IOList.Doc       as K
import qualified Koshucode.IOList.IOList.File      as K
import qualified Koshucode.IOList.IOList.Markdown  as K
import qualified Koshucode.IOList.IOList.Section   as K

-- | Create I/O list as lazy bytestring.
ioList :: K.Param -> [K.CmdLine] -> IO K.MixText
ioList p@K.Param {..} cmdlines = do
    cs <- mapM run cmdlines
    return $ ioListMix p cs

ioListMix :: K.Param -> [K.Section K.Command] -> K.MixText
ioListMix p@K.Param {..} cs = do
  K.toMarkdown $ K.numberingRoot $
             K.Doc { K.docParam   = p
                   , K.docProg    = paramProg
                   , K.docArgs    = paramArgs
                   , K.docCommand = cs }

run :: K.CmdLine -> IO (K.Section K.Command)
run cmdline =
    case words cmdline of
      []         -> return $ K.section "" K.NoCommand
      cmd : args -> do globbed <- globArgs args
                       case splitArgs globbed of
                         (args', []) -> single   cmd args'
                         argPair     -> multiple cmd argPair

-- | Rum command.
single :: K.CmdName -> [K.CmdArg] -> IO (K.Section K.Command)
single cmd args = do
  files  <- K.fileSections args
  result <- Proc.readProcessWithExitCode cmd args ""
  let cmdline = unwords $ cmd : args
      proc    = K.subsection "Output" $ K.Process cmdline result
  return $ K.section cmdline $ K.SingleCommand cmdline files proc

-- | Rum command with input files.
multiple :: K.CmdName -> ArgPair -> IO (K.Section K.Command)
multiple cmd (common, inputs) = do
  files  <- K.fileSections common
  ios    <- each cmd common `mapM` inputs
  let cmdline = unwords $ cmd : common ++ ["..."]
  return $ K.section cmdline $ K.MultipleCommands cmdline files ios

each :: K.CmdName -> [K.CmdArg] -> K.CmdArg -> IO (K.Section K.ArgProcess)
each cmd common input = do
  let args     = common ++ [input]
      cmdline  = unwords $ cmd : args
  input' <- K.maybeFile input
  result <- Proc.readProcessWithExitCode cmd args ""
  return $ K.subsection input $ K.ArgProcess input input' $ K.Process cmdline result


-- --------------------------------------------  glob and split

globArgs :: [K.CmdArg] -> IO [K.CmdArg]
globArgs args = do args' <- mapM globArg args
                   return $ concat args'

globArg :: K.CmdArg -> IO [K.CmdArg]
globArg arg = do
  arg' <- globFile arg
  return $ case arg' of
             [] -> [arg]
             _  -> arg'

globFile :: K.CmdArg -> IO [K.CmdArg]
globFile arg = do
  files <- K.listDirectory "."
  return $ filter (Glob.match $ Glob.compile arg) files

-- | Splitted arguments
type ArgPair = ([K.CmdArg], [K.CmdArg])

-- split arguments at last "//" into common args and input args
-- i.e., common ... // input ...
splitArgs :: [K.CmdArg] -> ArgPair
splitArgs xs = p1 $ break (== "//") $ reverse xs where
    p1 (input, "//" : common) = p2 common input
    p1 (common, [])           = p2 common []
    p1 (input, common)        = p2 common input
    p2 common input           = (reverse common, reverse (input \\ common))


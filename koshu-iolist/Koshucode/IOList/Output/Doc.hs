{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

-- | I/O list document.

module Koshucode.IOList.Output.Doc
 ( -- * Command list
   Doc (..),
   Command (..),
   -- * Input and Output
   ProcessResult,
   Process (..),
   ArgProcess (..),
 ) where

import qualified System.Exit                       as Exit
import qualified Koshucode.Baala.Base              as K

import qualified Koshucode.IOList.Parts            as K
import qualified Koshucode.IOList.Output.File      as K
import qualified Koshucode.IOList.Output.Markdown  as K
import qualified Koshucode.IOList.Output.Section   as K


-- ----------------------  Command list

-- | I/O list document.
data Doc = Doc
    { docParam    :: K.Param
    , docProg     :: String
    , docArgs     :: [String]
    , docCommand  :: [K.Section Command]
    } deriving (Show, Eq, Ord)

-- | Command in I/O list.
data Command
    = NoCommand
      -- ^ Empty command.

    | SingleCommand
        K.CmdLine
        [K.Section K.File]
        (K.Section Process)
      -- ^ Ordinary single command.

    | MultipleCommands
        K.CmdLine
        [K.Section K.File]
        [K.Section ArgProcess]
      -- ^ Command with multiple input files.

      deriving (Show, Eq, Ord)


-- ----------------------  Input and Output

-- | Exit status, standard output, and standard error.
type ProcessResult = (Exit.ExitCode, K.Bs, K.Bs)

-- | Process result.
data Process = Process
    { processCmdLine  :: K.CmdLine
    , processResult   :: ProcessResult
    } deriving (Show, Eq, Ord)

-- | Input file and process result
data ArgProcess = ArgProcess
    { apArg      :: String
    , apFile     :: Maybe K.File
    , apProcess  :: Process
    } deriving (Show, Eq, Ord)


-- ----------------------  Numbering

instance K.Numbering ArgProcess where
    numbering a = a

instance K.Numbering Process where
    numbering a = a

instance K.Numbering Doc where
    numbering (_, doc@Doc {..}) =
        case K.numbering (num docCommand, docCommand) of
          (ns', cmd') -> (ns', doc { docCommand = cmd' })
        where
          num [_] = []
          num _   = [1]

instance K.Numbering Command where
    numbering x@(_, NoCommand) = x

    numbering x@(_, SingleCommand _ [] _) = x
    numbering (ns, SingleCommand cmdline files proc) =
        case K.numbering (K.sectionDeepen ns, files) of
          (ns', files') -> case K.numbering (ns', proc) of
                             (ns'', proc') -> (ns'', SingleCommand cmdline files' proc')

    numbering x@(_, MultipleCommands _ [] [_]) = x
    numbering (ns, MultipleCommands cmdline files ios) =
        case K.numbering (K.sectionDeepen ns, files) of
          (ns', files') -> case K.numbering (ns', ios) of
                             (ns'', ios') -> (ns'', MultipleCommands cmdline files' ios')
                                

-- ----------------------  ToMarkdown

instance K.ToMarkdown Doc where
    toMarkdown Doc {..} =
        K.mdTitle docParam   -- unwords (docProg : docArgs)
        K.<> K.toMarkdown docCommand

instance K.ToMarkdown Command where

    toMarkdown NoCommand = K.mixEmpty

    --  Single command
    --
    --  1. Command
    --     1.1 File in command
    --     1.2 File in command
    --     1.3 Output
    --
    toMarkdown (SingleCommand _ files proc) =
        K.toMarkdown files K.<> K.toMarkdown proc

    --  Multiple commands
    --
    --  1. Command
    --     1.1 File in command
    --     1.2 File in command
    --     1.3 File and Output
    --     1.4 File and Output
    --
    toMarkdown (MultipleCommands _ files ios) =
        K.toMarkdown files K.<> K.toMarkdown ios

instance K.ToMarkdown ArgProcess where
    toMarkdown ArgProcess {..} =
      K.toMarkdown apFile
      K.<> K.toMarkdown apProcess

instance K.ToMarkdown Process where
    toMarkdown (Process cmdline (exit, stdout, stderr)) =
        K.mdStatus cmdline exit
        K.<> K.mdBlock stdout
        K.<> mdStderr stderr

mdStderr :: K.Bs -> K.MixText
mdStderr "" = K.mixEmpty
mdStderr bs =
  K.mixLine (K.mixBs "(standard error)")
  K.<> K.mixHard
  K.<> K.mdBlock bs

